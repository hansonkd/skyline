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

        %NoRouteError{message: "no route found for #{conn.method} #{path} (#{inspect router})",
                      conn: conn, router: router}
      end
    end

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
       Module.register_attribute __MODULE__, :phoenix_routes, accumulate: true

       #import Skyline.Topic.Pipeline
       import Skyline.Topic.Router
       import Skyline.Topic.Router.Route

       # Set up initial scope
       @phoenix_pipeline nil
       Skyline.Topic.Scope.init(__MODULE__)
       @before_compile unquote(__MODULE__)
     end
   end

   defp match_dispatch() do
     quote location: :keep do
       #@behaviour Pipe

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

       def to_function(opts \\ nil) do
         nopts = init(opts)
         fn(conn) -> call(conn, nopts) end
       end

       defp match_route(conn, []) do
         match_route(conn, conn.method, Enum.map(conn.path_info, &URI.decode/1))
       end

       defp dispatch(conn, []) do
         try do
           conn.private.phoenix_route.(conn)
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
          IO.inspect pipe
          @phoenix_pipeline []
          unquote(block)
        end

      compiler =
        quote unquote: false do
          Skyline.Topic.Scope.pipeline(__MODULE__, pipe)
          IO.inspect  @phoenix_pipeline
          {conn, body} = Skyline.Topic.Pipe.compile(__ENV__, @phoenix_pipeline, [])
          def unquote(pipe)(unquote(conn), _) do
            try do
              unquote(body)
            catch
              kind, reason ->
                Skyline.WrapperError.reraise(unquote(conn), kind, reason)
            end
          end
          @phoenix_pipeline nil
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
      routes = env.module |> Module.get_attribute(:phoenix_routes) |> Enum.reverse
      routes_with_exprs = Enum.map(routes, &{&1, Skyline.Topic.Router.Route.exprs(&1)})

      #Helpers.define(env, routes_with_exprs)
      matches = Enum.map(routes_with_exprs, &build_match/1)

      pipes = [{:dispatch, [], true}, {:match_route, [], true}]
      {conn, pipeline} = Skyline.Topic.Pipe.compile(env, pipes, [])

      call =
        quote do
          unquote(conn) =
            update_in unquote(conn).private,
              &(&1 |> Map.put(:phoenix_pipelines, [])
                   |> Map.put(:phoenix_router, __MODULE__))
          unquote(pipeline)
        end

      # line: -1 is used here to avoid warnings if forwarding to root path
      match_404 =
        quote line: -1 do
          defp match_route(conn, _method, _path_info) do
            raise NoRouteError, conn: conn, router: __MODULE__
          end
        end

      quote do
        defp do_call(unquote(conn), opts) do
          unquote(call)
        end

        @doc false
        def __routes__,  do: unquote(Macro.escape(routes))

        @doc false
        def __helpers__, do: __MODULE__.Helpers

        unquote(matches)
        unquote(match_404)
      end
    end

    defp build_match({route, exprs}) do
      IO.inspect(exprs.path)
      IO.inspect(exprs.verb_match)
      quote do

        defp match_route(var!(conn), unquote(exprs.verb_match),
                        unquote(exprs.path)) do

          unquote(exprs.dispatch)


        end
      end
    end

    defmacro match(verb, path, pipe, pipe_opts, options \\ []) do
      add_route(verb, path, pipe, pipe_opts, options)
    end

    for verb <- @incoming_message_types do
     @doc """
     Generates a route to handle a #{verb} request to the given path.
     """
     defmacro unquote(verb)(path, pipe, options \\ []) do
       verb = unquote(verb)
       quote bind_quoted: binding do
         match(verb, path, pipe, verb, options)
       end
     end
    end

    defp add_route(verb, path, pipe, pipe_opts, options) do
      quote do
        @phoenix_routes Skyline.Topic.Scope.route(__MODULE__, unquote(verb), unquote(path),
                                    unquote(pipe), unquote(pipe_opts), unquote(options))
      end
    end

    defmacro pipe_through(pipes) do
        quote do
          if pipeline = @phoenix_pipeline do
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
        if pipeline = @phoenix_pipeline do
          @phoenix_pipeline [{unquote(pipe), unquote(opts), true}|pipeline]
        else
          raise "cannot define pipe at the router level, pipe must be defined inside a pipeline"
        end
      end
    end
end
