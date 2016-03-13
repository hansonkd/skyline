defmodule SpotApp.Worker do

  import Socket
  alias Spotmq.Session
  alias Spotmq.Msg.Decode.Utils, as: Decoder
  alias Spotmq.Handler

  def start_link(default) do
    opts = [port: 8000]
    IO.puts("Starting rancher")
    :ranch.start_listener(:Spotmq, 100, :ranch_tcp, opts, Spotmq.Listener, [])
  end

end
