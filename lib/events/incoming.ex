defmodule Skyline.Events.Incoming do

  use GenEvent

  def start_link() do
    GenEvent.start_link(name: :skyline_incoming)
  end


  def add_handler(handler, init) do
    GenEvent.add_handler(:skyline_incoming, handler, init)
  end

end
