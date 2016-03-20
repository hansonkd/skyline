defmodule Skyline.Topic.Controller.Default.Subscribe do
  use Skyline.Topic.Controller
  alias Skyline.Topic.Conn

  def init(opts) do
    opts
  end

  def subscribe(%Conn{message: msg, client: client}, _opts) do
    Skyline.Topic.Utils.subscribe(msg, client)
    :ok
  end

end
