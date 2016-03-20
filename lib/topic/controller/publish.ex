defmodule Skyline.Topic.Controller.Publish do
  @callback init(any) :: any
  @callback publish(Skyline.Topic.Conn.t, any) :: Skyline.Topic.Controller.response
end
