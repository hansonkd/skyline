defmodule Skyline.Events.Outgoing do

  use GenEvent

  def start_link() do
    GenEvent.start_link(name: :skyline_outgoing)
  end


  def add_handler(handler, init) do
    GenEvent.add_handler(:skyline_outgoing, handler, init)
  end

end
