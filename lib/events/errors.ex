defmodule Skyline.Events.Errors do

  use GenEvent

  def start_link() do
    GenEvent.start_link(name: :skyline_errors)
  end

  def add_handler(handler, init) do
    GenEvent.add_handler(:skyline_errors, handler, init)
  end

end
