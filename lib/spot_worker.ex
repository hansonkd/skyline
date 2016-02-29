defmodule SpotApp.Worker do

  import Socket

  def start_link(default) do
    pid = spawn_link(fn -> init(8000) end)
    {:ok, pid}
  end



  def init(port) do
    server = Socket.TCP.listen!(port, [packet: 0,
											                 reuseaddr: true,
											                 nodelay: true,
											                 active: false])
    do_listen(server)
  end


  defp do_listen(server) do
    client = server |> Socket.TCP.accept!

    {:ok, pid} = SpotSender.start_link(client)
    spawn(fn() -> do_server(client, pid) end)

    do_listen(server)
  end

  defp do_server(client, pid) do



    case Socket.Stream.recv(client, 2) do

      { :ok, data } ->
        #GenServer.cast(pid, {:msg, data})
        #msg = Spotmq.Decoder.decode(data, read_byte(client), fn n -> read_bytes(client, n) end)

        GenServer.cast({:via, :gproc, {:p, :l, :my_little_server}}, {:msg, data})

        #:gen_tcp.send(socket, data)
        do_server(client, pid)

      { :error, :closed } -> :ok
      other -> IO.inspect other
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
