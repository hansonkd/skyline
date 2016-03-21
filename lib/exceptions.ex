defmodule Skyline.AlreadyConnected do
  @moduledoc """
  Exception raised when client tries to reconnect in same socket.
  """
  defexception message: "already connected", client_id: nil
  alias Skyline.AlreadyConnected

  def exception(opts) do
    client_id   = Keyword.fetch!(opts, :client_id)

    %AlreadyConnected{message: "#{client_id} was already connected.",
                      client_id: client_id}
  end
end
defmodule Skyline.MalformedMessage do
  @moduledoc """
  Exception raised when incoming message cannot be read.
  """
  defexception message: "already connected", bytes_recieved: nil
  alias Skyline.MalformedMessage

  def exception(opts) do
    bytes_recieved = Keyword.fetch!(opts, :bytes_recieved)

    %MalformedMessage{message: "#{bytes_recieved} was not able to be decoded.",
                      bytes_recieved: bytes_recieved}
  end
end
defmodule Skyline.QosError do
  @moduledoc """
  Exception raised when there was an error during the Qos process.
  """
  defexception message: "qos error", expected_msg_type: nil, expected_id: nil,
               recv_msg_type: nil, recv_msg_id: nil

  alias Skyline.QosError

  def exception(opts) do
    expected_msg_type = Keyword.fetch!(opts, :expected_msg_type)
    expected_id = Keyword.fetch!(opts, :expected_id)
    recv_msg_type = Keyword.fetch!(opts, :recv_msg_type)
    recv_msg_id = Keyword.fetch!(opts, :recv_msg_id)

    %QosError{message: "Expected message #{expected_msg_type} with id \##{expected_id}. Instead got #{recv_msg_type} with id \##{recv_msg_id}",
              expected_msg_type: expected_msg_type,
              expected_id: expected_id,
              recv_msg_type: recv_msg_type,
              recv_msg_id: recv_msg_id,
              }
  end
end
defmodule Skyline.ActionClauseError do
  defexception [message: nil]

  def exception(opts) do
    controller = Keyword.fetch!(opts, :controller)
    action = Keyword.fetch!(opts, :action)
    msg = "bad request to #{inspect controller}.#{action}, " <>
          "no matching action clause to process message"
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

