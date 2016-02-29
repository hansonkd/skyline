defmodule SpotApp.WorkerWorking do

  def start_link(default) do
    pid = spawn_link(fn -> init(8000) end)
    {:ok, pid}
  end

  def init(port) do
    tcp_options = [:list, {:packet, 0}, {:active, false}, {:reuseaddr, true}]
    {:ok, l_socket} = :gen_tcp.listen(port, tcp_options)

    do_listen(l_socket)
  end


  defp do_listen(l_socket) do
    {:ok, socket} = :gen_tcp.accept(l_socket)

    {:ok, pid} = SpotSender.start_link(socket)
    spawn(fn() -> do_server(socket, pid) end)

    do_listen(l_socket)
  end

  defp do_server(socket, pid) do
    case :gen_tcp.recv(socket, 0) do

      { :ok, data } ->
        #GenServer.cast(pid, {:msg, data})
        GenServer.cast({:via, :gproc, {:p, :l, :my_little_server}}, {:msg, data})

        #:gen_tcp.send(socket, data)
        do_server(socket, pid)

      { :error, :closed } -> :ok
    end
  end
end
