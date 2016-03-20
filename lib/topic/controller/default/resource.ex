defmodule Skyline.Topic.Controller.Default.ResourceController do
  use Skyline.Topic.Controller
  alias Skyline.Topic.Conn

  def init(opts) do
    opts
  end

  def publish(%Conn{} = conn, _opts) do
    conn
  end

  def subscribe(%Conn{} = conn, _opts) do
    conn
  end

end
