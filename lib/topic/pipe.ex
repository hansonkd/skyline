defmodule Skyline.Topic.Pipe do
  @moduledoc false

  #@spec compile(Macro.Env.t, [{pipe, Skyline.Topic.Pipe.opts, Macro.t}], Keyword.t) :: {Macro.t, Macro.t}
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
       {:ok, qos} = n -> n
       :ok -> :ok
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
