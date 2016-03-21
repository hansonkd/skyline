defmodule Skyline.Events.Auth do

  use GenEvent

  def start_link() do
    GenEvent.start_link(name: :skyline_auth_events)
  end

  def add_handler(handler, init) do
    GenEvent.add_handler(:skyline_auth_events, handler, init)
  end

end
