defmodule Skyline.Acceptor do
  # A worker process which accepts new connections and starts the listeners

  @moduledoc false

  import Socket
  alias Skyline.Session
  alias Skyline.Msg.Decode.Utils, as: Decoder
  alias Skyline.Handler

  import Supervisor.Spec

  def start_link(app, port) do
    pid = spawn_link(fn -> init(app, port) end)
    {:ok, pid}
  end

  def init(app, port) do
    server = Skyline.Socket.listen(port)
    do_listen(server, app)
  end

  defp do_listen(server, app) do
    client = server |> Socket.TCP.accept!

    {:ok, _pid} = Skyline.Client.start_link(client, app)

    do_listen(server, app)

  end

end
