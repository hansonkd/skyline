defmodule SpotSender do
  use GenServer

  import Socket

  def start_link(socket, opts \\ []) do
    GenServer.start_link(__MODULE__, [socket: socket], opts)
  end

  def init(socket) do
    :gproc.reg({:p, :l, :my_little_server})
    {:ok, socket}
  end

  # def handle_call(:pop, _from, [socket: socket]) do
  #   {:reply, h, t}
  # end

  def handle_cast({:msg, msg}, [socket: socket] = state) do
    socket |> Socket.Stream.send(msg)
    {:noreply, state}
  end
end
