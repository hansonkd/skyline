defmodule Skyline.Acceptor do
  @moduledoc """
  A worker process which accepts new connections and starts the listeners
  """
  import Socket
  alias Skyline.Session
  alias Skyline.Msg.Decode.Utils, as: Decoder
  alias Skyline.Handler

  import Supervisor.Spec

  def start_link(default) do
    pid = spawn_link(fn -> init(8000) end)
    {:ok, pid}
  end

  def init(port) do
    server = Socket.TCP.listen!(port, [packet: 0,
											                 reuseaddr: true,
                                       binary: true,
                                       nodelay: true,
                                       active: true,
                                       backlog: 30,
											                 mode: :active])
    do_listen(server)
  end

  defp do_listen(server) do
    client = server |> Socket.TCP.accept!


    {:ok, _pid} = Skyline.Listener.start_link(client)

    do_listen(server)

  end

end
