defmodule Example.Controller.NoMatch do
  use Skyline.Topic.Controller

  def init(opts) do
    opts
  end

  def publish(_conn, _opts) do
    {:close_connection, "Not a prechosen topic."}
  end

  def subscribe(_conn, _opts) do
    {:close_connection, "Not a prechosen topic."}
  end

end
