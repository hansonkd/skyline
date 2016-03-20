defmodule Skyline.Topic.Controller.Default.SubscribeController do
  use Skyline.Topic.Controller
  alias Skyline.Topic.Conn

  def init(opts) do
    opts
  end

  def subscribe(%Conn{topic: topic, qos: qos, message: msg, client: client}, _opts) do
    ret_qos = Skyline.Topic.Utils.subscribe(topic, qos, msg, client)
    {:ok, ret_qos}
  end

end
