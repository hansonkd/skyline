defmodule SpotApp.Worker do

  import Socket
  alias Spotmq.Session
  alias Spotmq.Decoder
  alias Spotmq.Handler

  def start_link(default) do
    pid = spawn_link(fn -> init(8000) end)
    {:ok, pid}
  end

  def init(port) do
    server = Socket.TCP.listen!(port, [packet: 0,
											                 reuseaddr: true,
                                       binary: true,
                                       nodelay: false,
											                 mode: :active])
    do_listen(server)
  end

  defp do_listen(server) do
    client = server |> Socket.TCP.accept!

    spawn(fn() -> wait_for_connection(client) end)

    do_listen(server)
  end

  defp wait_for_connection(client) do
    case Socket.Stream.recv(client, 2) do
      { :ok, data = <<_m :: size(16)>> } ->
          msg = Decoder.decode(data, read_byte(client), fn n -> read_bytes(client, n) end)
          case msg do
            %Spotmq.Msg.Connection{} -> authenticate(client, msg)
            _other -> IO.inspect("Have not recieved auth")
                      wait_for_connection(client)
          end
      {:error, :closed } -> :ok
      {:error, :enotconn} -> wait_for_connection(client)
      other -> IO.inspect other
               wait_for_connection(client)
    end
  end

  defp authenticated_loop(client, sess_pid) do
    case Socket.Stream.recv(client, 2) do
      { :ok, data = <<_m :: size(16)>> } ->
          msg = Decoder.decode(data, read_byte(client), fn n -> read_bytes(client, n) end)
          case msg do
            %Spotmq.Msg.Connection{} ->
                IO.inspect("Only one connect message per connection...")
                :ok
            _other -> Spotmq.Handler.handle_msg(msg, sess_pid)
                      authenticated_loop(client, sess_pid)
          end
      {:error, :closed } -> :ok
      other -> IO.inspect other
               :ok
    end
  end

  defp authenticate(client, %Spotmq.Msg.Connection{client_id: client_id} = msg) do

      case Spotmq.Auth.connect(client, msg) do
        {:ok, smsg, sess_pid} -> Session.send_to_socket(client, smsg)
                       authenticated_loop(client, sess_pid)
        {:error, emsg} -> Session.send_to_socket(client, emsg)
                          wait_for_connection(client)
      end
  end

@doc "Returns a function that reads a single byte from the socket and crashes if problems occur"
defp read_byte(socket) do
  fn ->
    case Socket.Stream.recv(socket, 1) do
      {:ok, byte} -> {byte, read_byte(socket)}
      {:error, reason} -> IO.inspect("read_byte: receiving 1 byte failed with #{inspect reason}")
    end
  end
end

@doc "Reads bytes from the socket and crashes if problems occur"
defp read_bytes(socket, 0), do: ""
defp read_bytes(socket, nr) do
    IO.inspect("read_bytes: #{nr} bytes from socket #{inspect socket}")
    result = case Socket.Stream.recv(socket, nr) do
      {:ok, bytes} -> bytes
      # {:error, reason} -> Lager.error("read_bytes: receiving #{nr} bytes failed with #{inspect reason}")
      any ->
        IO.inspect("Received a strange message: #{inspect any}")
        any
    end
    IO.inspect("read_bytes: result = #{inspect result}")
    result
  end

end
