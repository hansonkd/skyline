defmodule Skyline.Topic.Pipe do
  @moduledoc """
    The pipe specification for transforming `Skyline.Conn`.
    
    There are two kind of pipes: function pipes and module pipes.
    #### Function pipes
    A function pipe is any function that receives a connection and a set of
    options and returns a connection. Its type signature must be:
        (Skyline.Conn.t, Plug.opts) :: Plug.Conn.t
    #### Module pipes
    A module pipe is an extension of the function pipe. It is a module that must
    export:
    * a `call/2` function wipipeth the signature defined above
    * an `init/1` function which takes a set of options and initializes it.
    The result returned by `init/1` is passed as second argument to `call/2`. Note
    that `init/1` may be called during compilation and as such it must not return
    pids, ports or values that are not specific to the runtime.
    The API expected by a module pipe is defined as a behaviour by the
    `pipe` module (this module).
    ## Examples
    Here's an example of a function pipe:
        def lowercase_topic(%Conn{topic: topic} = conn, opts) do
          %{conn | topic: String.downcase(topic)}
        end
    Here's an example of a module pipe:
        defmodule LowercaseTopicPipe do
          def init(opts) do
            opts
          end
          def call(%Conn{topic: topic} = conn, _opts) do
             %{conn | topic: String.downcase(topic)}
          end
        end
    """
  
  @type opts :: tuple | atom | integer | float | [opts]
  @type response :: :ok | {:close_connection, String.t} | :do_nothing | {:topic_qos, Skyline.qos_type}
  @type pipe :: module | atom
  
  use Behaviour
  
  @callback init(term) :: term
  @callback call(Skyline.Conn.t, term) :: response
  

  @spec compile(Macro.Env.t, [{pipe, Skyline.Topic.Pipe.opts, Macro.t}], Keyword.t) :: {Macro.t, Macro.t}
  def compile(env, pipeline, builder_opts) do
   conn = quote do: conn
   {conn, Enum.reduce(pipeline, conn, &quote_pipe(init_pipe(&1), &2, env, builder_opts))}
  end

  # Initializes the options of a pipe at compile time.
  defp init_pipe({pipe, opts, guards}) do
   case Atom.to_char_list(pipe) do
     'Elixir.' ++ _ -> init_module_pipe(pipe, opts, guards)
     _              -> init_fun_pipe(pipe, opts, guards)
   end
  end

  defp init_module_pipe(pipe, opts, guards) do
   initialized_opts = pipe.init(opts)

   if function_exported?(pipe, :call, 2) do
     {:module, pipe, initialized_opts, guards}
   else
     raise ArgumentError, message: "#{inspect pipe} pipe must implement call/2"
   end
  end

  defp init_fun_pipe(pipe, opts, guards) do
   {:function, pipe, opts, guards}
  end

  # `acc` is a series of nested pipe calls in the form of
  # pipe3(pipe2(pipe1(conn))). `quote_pipe` wraps a new pipe around that series
  # of calls.
  defp quote_pipe({pipe_type, pipe, opts, guards}, acc, env, builder_opts) do
   call = quote_pipe_call(pipe_type, pipe, opts)

   error_message = case pipe_type do
     :module   -> "expected #{inspect pipe}.call/2 to return a Skyline.Topic.Conn"
     :function -> "expected #{pipe}/2 to return a Skyline.Topic.Conn"
   end <> ", all pipes must receive a connection (conn) and return a connection"

   quote do
     case unquote(compile_guards(call, guards)) do
       {:close_connection, _reason} = ret ->
         unquote(log_halt(pipe_type, pipe, env, builder_opts))
         ret
       %Skyline.Topic.Conn{} = conn ->
         unquote(acc)
       {:topic_qos, qos} = n -> n
       :do_nothing -> :do_nothing
       a ->
         raise unquote(error_message)
     end
   end
  end

  defp quote_pipe_call(:function, pipe, opts) do
   quote do: unquote(pipe)(conn, unquote(Macro.escape(opts)))
  end

  defp quote_pipe_call(:module, pipe, opts) do
   quote do: unquote(pipe).call(conn, unquote(Macro.escape(opts)))
  end

  defp compile_guards(call, true) do
   call
  end

  defp compile_guards(call, guards) do
   quote do
     case true do
       true when unquote(guards) -> unquote(call)
       true -> conn
     end
   end
  end

  defp log_halt(pipe_type, pipe, env, builder_opts) do
   if level = builder_opts[:log_on_halt] do
     message = case pipe_type do
       :module   -> "#{inspect env.module} halted in #{inspect pipe}.call/2"
       :function -> "#{inspect env.module} halted in #{inspect pipe}/2"
     end

     quote do
       require Logger
       Logger.unquote(level)(unquote(message))
     end
   else
     nil
   end
  end

end
