defmodule Skyline.Topic.Controller.Subscribe do
  @callback init(any) :: any
  @callback subscribe(Skyline.Topic.Conn.t, any) :: Skyline.Topic.Controller.response
end
