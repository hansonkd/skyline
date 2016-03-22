defmodule Skyline.Acceptor do
  @moduledoc false

  # A worker process which accepts new connections and starts the listeners



  import Socket
  alias Skyline.Session
  alias Skyline.Msg.Decode.Utils, as: Decoder
  alias Skyline.Handler

  import Supervisor.Spec

  def start_link(app_config, port) do
    pid = spawn_link(fn -> init(app_config, port) end)
    {:ok, pid}
  end

  def init(app_config, port) do
    server = Skyline.Socket.listen(port)
    do_listen(server, app_config)
  end

  defp do_listen(server, app_config) do
    client = server |> Socket.TCP.accept!

    {:ok, _pid} = Skyline.Client.start_link(client, app_config)

    do_listen(server, app_config)

  end

end
