defmodule Skyline.Topic.Controller.Default.PublishController do
  use Skyline.Topic.Controller
  alias Skyline.Topic.Conn

  def init(opts) do
    opts
  end

  def publish(%Conn{message: msg, topic: topic, qos: qos, client: client}, _opts) do
    Skyline.Topic.Utils.publish(topic, qos, msg, client)
    :ok
  end

end
