defmodule Skyline.Topic.Controller.Default.SubscribeController do
  use Skyline.Topic.Controller
  alias Skyline.Topic.Conn

  def init(opts) do
    opts
  end

  def subscribe(%Conn{} = conn, _opts) do
    conn
  end

end
