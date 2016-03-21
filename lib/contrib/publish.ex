defmodule Skyline.Contrib.PublishController do
  @moduledoc "A pass-through publish controller that publishes all messages as it recieves them."
  
  use Skyline.Topic.Controller
  
  alias Skyline.Topic.Conn

  def init(opts) do
    opts
  end

  def publish(%Conn{} = conn, _opts) do
    conn
  end

end
