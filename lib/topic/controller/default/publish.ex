defmodule Skyline.Topic.Controller.Default.Publish do
  use Skyline.Topic.Controller
  alias Skyline.Topic.Conn

  def init(opts) do
    opts
  end

  def publish(%Conn{message: msg, client: client}, _opts) do
    Skyline.Topic.Utils.publish(msg, client)
    :ok
  end

end
