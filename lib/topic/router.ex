defmodule Skyline.Topic.Router do
    
    defmodule NoRouteError do
      @moduledoc """
      Exception raised when no route is found.
      """
      defexception message: "no route found", conn: nil, router: nil

      def exception(opts) do
        conn   = Keyword.fetch!(opts, :conn)
        router = Keyword.fetch!(opts, :router)
        path   = "/" <> Enum.join(conn.path_info, "/")

        %NoRouteError{message: "no route found for #{conn.action} #{path} (#{inspect router})",
                      conn: conn, router: router}
      end
    end

    @moduledoc """
     Defines a Skyline router.
     
     Skyline router is a simplified version of Phoenix's so don't be 
     surprised if there are simularities.
     The router provides a set of macros for generating routes
     that dispatch to specific controllers and actions. Those
     macros are named after message types publish and subscribe. For example:
         defmodule MyApp.Router do
           use Skyline.Router
           publish "/topic/:topic", PublishController
         end
     The `publish/2` macro above accepts a request of format `"/topic/VALUE"` and
     dispatches it to `PublishController.publish/2`.
     Routes can also match glob-like patterns, routing any path with a common
     base to the same controller. For example:
         publish "/dynamic*anything", DynamicController
     
     Skyline's router is extremely efficient, as it relies on Elixir
     pattern matching for matching routes and serving requests.

     ### Scopes and Resources
     The router also supports scoping of routes:
         scope "/api/v1" do
           subscribe "/devices/:id", DeviceController
         end
     For example, the route above will match on the path `"/api/v1/devices/:id"`
     and the named route will be `api_v1_page_path`, as expected from the
     values given to `scope/2` option.
     Skyline also provides a `resources/2` macro that allows developers
     to shortcut `publish/3` and `subscribe/3`:
         defmodule MyApp.Router do
           use Skyline.Router
           resources "/devices", DeviceController
           resources "/users", UserController
         end
     Check `scope/2` and `resources/2` for more information.
     ## Pipelines and pipes
     Once a request arrives at the Skyline router, it performs
     a series of transformations through pipelines until the
     request is dispatched to a desired end-point.
     Such transformations are defined via pipes, as defined
     in `Skyline.Pipe`.
     Once a pipeline is defined, it can be piped through per scope.
     For example:
         defmodule MyApp.Router do
           use Skyline.Router
           pipeline :session do
             pipe :fetch_session
           end
           scope "/" do
             pipe_through :session
             # session related routes and resources
           end
         end
     `Skyline.Router` imports functions from both `Skyline.Conn` and `Skyline.Controller`
     to help define pipes. In the example above, `fetch_session/2`
     comes from `Skyline.Conn` while `accepts/2` comes from `Skyline.Controller`.
     Note that router pipelines are only invoked after a route is found.
     No pipe is invoked in case no matches were found.
     """
     
    @incoming_message_types [:publish, :subscribe]
    import Skyline.Topic.Scope
    
    

    @doc false
    defmacro __using__(_) do
      quote do
        unquote(prelude())
        #unquote(defs())
        unquote(match_dispatch())
      end
    end

    defp prelude() do
     quote do
       Module.register_attribute __MODULE__, :skyline_routes, accumulate: true
       
       @behavior Skyline.Topic.Pipe
       
       import Skyline.Topic.Router
       import Skyline.Topic.Router.Route

       # Set up initial scope
       @skyline_pipeline nil
       Skyline.Topic.Scope.init(__MODULE__)
       @before_compile unquote(__MODULE__)
     end
   end

   defp match_dispatch() do
     quote location: :keep do
       @behaviour Skyline.Topic.Pipe

       @doc """
       Callback required by Pipe that initializes the router
       for serving web requests.
       """
       def init(opts) do
         opts
       end

       @doc """
       Callback invoked by Pipe on every request.
       """
       def call(conn, opts), do: do_call(conn, opts)

       defp match_route(conn, []) do
         match_route(conn, conn.action, Enum.map(conn.path_info, &URI.decode/1))
       end

       defp dispatch(conn, []) do
         try do
           conn.private.skyline_route.(conn)
         catch
           kind, reason ->
             Skyline.WrapperError.reraise(conn, kind, reason)
         end
       end

       defoverridable [init: 1, call: 2]
     end
   end

   defmacro pipeline(pipe, do: block) do
      block =
        quote do
          pipe = unquote(pipe)
          @skyline_pipeline []
          unquote(block)
        end

      compiler =
        quote unquote: false do
          Skyline.Topic.Scope.pipeline(__MODULE__, pipe)
          {conn, body} = Skyline.Topic.Pipe.compile(__ENV__, @skyline_pipeline, [])
          def unquote(pipe)(unquote(conn), _) do
            try do
              unquote(body)
            catch
              kind, reason ->
                Skyline.WrapperError.reraise(unquote(conn), kind, reason)
            end
          end
          @skyline_pipeline nil
        end

      quote do
        try do
          unquote(block)
          unquote(compiler)
        after
          :ok
        end
      end
    end

    defmacro __before_compile__(env) do
      routes = env.module |> Module.get_attribute(:skyline_routes) |> Enum.reverse
      routes_with_exprs = Enum.map(routes, &{&1, Skyline.Topic.Router.Route.exprs(&1)})

      #Helpers.define(env, routes_with_exprs)
      matches = Enum.map(routes_with_exprs, &build_match/1)

      pipes = [{:dispatch, [], true}, {:match_route, [], true}]
      {conn, pipeline} = Skyline.Topic.Pipe.compile(env, pipes, [])

      call =
        quote do
          unquote(conn) =
            update_in unquote(conn).private,
              &(&1 |> Map.put(:skyline_pipelines, [])
                   |> Map.put(:skyline_router, __MODULE__))
          unquote(pipeline)
        end

      # line: -1 is used here to avoid warnings if forwarding to root path
      no_match =
        quote line: -1 do
          defp match_route(conn, _action, _path_info) do
            raise NoRouteError, conn: conn, router: __MODULE__
          end
        end

      quote do
        defp do_call(unquote(conn), opts) do
          unquote(call)
        end

        unquote(matches)
        unquote(no_match)
      end
    end

    defp build_match({_route, exprs}) do
      quote do
        defp match_route(var!(conn), unquote(exprs.verb_match),
                        unquote(exprs.path)) do
          unquote(exprs.dispatch)
        end
      end
    end

    defmacro match(verb, path, controller, pipe_opts, options \\ []) do
      add_route(verb, path, controller, pipe_opts, options)
    end

    for verb <- @incoming_message_types do
     @doc """
     Generates a route to handle a #{verb} request to the given path.
     """
     defmacro unquote(verb)(path, controller, options \\ []) do
       verb = unquote(verb)
       quote bind_quoted: binding do
         match(verb, path, controller, verb, options)
       end
     end
    end

    defmacro resource(path, pipe, options \\ []) do

      for verb <- @incoming_message_types do

        quote bind_quoted: binding do
          match(verb, path, pipe, verb, options)
        end
      end
    end

    defp add_route(verb, path, pipe, pipe_opts, options) do
      quote do
        @skyline_routes Skyline.Topic.Scope.route(__MODULE__, unquote(verb), unquote(path),
                                    unquote(pipe), unquote(pipe_opts), unquote(options))
      end
    end

    defmacro pipe_through(pipes) do
        quote do
          if pipeline = @skyline_pipeline do
            raise "cannot pipe_through inside a pipeline"
          else
            Skyline.Topic.Scope.pipe_through(__MODULE__, unquote(pipes))
          end
        end
    end


    defmacro scope(options, do: context) do
      do_scope(options, context)
    end

    @doc """
    Define a scope with the given path.
    This function is a shortcut for:
        scope path: path do
          ...
        end
    """
    defmacro scope(path, options, do: context) do
      options = quote do
        path = unquote(path)
        case unquote(options) do
          alias when is_atom(alias) -> [path: path, alias: alias]
          options when is_list(options) -> Keyword.put(options, :path, path)
        end
      end
      do_scope(options, context)
    end

    @doc """
    Defines a scope with the given path and alias.
    This function is a shortcut for:
        scope path: path, alias: alias do
          ...
        end
    """
    defmacro scope(path, alias, options, do: context) do
      options = quote do
        unquote(options)
        |> Keyword.put(:path, unquote(path))
        |> Keyword.put(:alias, unquote(alias))
      end
      do_scope(options, context)
    end

    defp do_scope(options, context) do
      quote do
        Skyline.Topic.Scope.push(__MODULE__, unquote(options))
        try do
          unquote(context)
        after
          Skyline.Topic.Scope.pop(__MODULE__)
        end
      end
    end

    defmacro pipe(pipe, opts \\ []) do
      quote do
        if pipeline = @skyline_pipeline do
          @skyline_pipeline [{unquote(pipe), unquote(opts), true}|pipeline]
        else
          raise "cannot define pipe at the router level, pipe must be defined inside a pipeline"
        end
      end
    end
end
