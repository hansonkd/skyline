defmodule Skyline.Contrib.SubscribeController do
   @moduledoc "A pass-through controller that subscribes to topics as it recieves them."
   
  use Skyline.Topic.Controller
  alias Skyline.Topic.Conn

  def init(opts) do
    opts
  end

  def subscribe(%Conn{} = conn, _opts) do
    conn
  end

end
