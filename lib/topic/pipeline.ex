defmodule Skyline.ActionClauseError do
  defexception [message: nil, plug_status: 400]

  def exception(opts) do
    controller = Keyword.fetch!(opts, :controller)
    action = Keyword.fetch!(opts, :action)
    msg = "bad request to #{inspect controller}.#{action}, " <>
          "no matching action clause to process request"
    %Skyline.ActionClauseError{message: msg}
  end
end
defmodule Skyline.WrapperError do
  @moduledoc """
  Wraps the connection in an error which is meant
  to be handled upper in the stack.
  Used by both `Pipe.Debugger` and `Pipe.ErrorHandler`.
  """
  defexception [:conn, :kind, :reason, :stack]

  def message(%{kind: kind, reason: reason, stack: stack}) do
    Exception.format_banner(kind, reason, stack)
  end

  @doc """
  Reraises an error or a wrapped one.
  """
  def reraise(_conn, :error, %__MODULE__{stack: stack} = reason) do
    :erlang.raise(:error, reason, stack)
  end

  def reraise(conn, kind, reason) do
    stack   = System.stacktrace
    wrapper = %__MODULE__{conn: conn, kind: kind, reason: reason, stack: stack}
    :erlang.raise(:error, wrapper, stack)
  end
end
