defmodule Example.Controller.LocationResource do
  use Skyline.Topic.Controller

  alias Skyline.Topic.Conn
  alias Skyline.Msg.PublishReq

  def init(opts) do
    opts
  end

  def publish(%Conn{message: %PublishReq{message: body},
                    params: %{"username" => username}} = conn, _opts) do
    IO.puts "User #{username} updated their location to #{body}"
    conn
  end

  def subscribe(%Conn{topic: topic, params: %{"username" => username}}=conn, _opts) do
    IO.puts "New subscription to #{username}'s topic #{topic}."
    conn
  end

end