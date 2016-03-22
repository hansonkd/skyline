defmodule Skyline.Contrib.ResourceController do
  @moduledoc """
     A pass-through resource controller that combines 
     `Skyline.Contrib.PublishController` with 
     `Skyline.Contrib.SubscribeController`
  """
  
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
