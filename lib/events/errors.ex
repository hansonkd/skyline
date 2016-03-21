defmodule Skyline.Events.Errors do
  @moduledoc """
  GenEvent server to handle client errors.
  
  An app can hook into the Errors event system by calling `add_handler/2`
      defmodule Example.EventHandler do
        use GenEvent
        require Logger
        alias Skyline.Events.Errors
        def init() do
            Errors.add_handler(Example.EventHandler, nil)
        end
        def handle_event({:error, {client_id, auth_info, exception}}, st) do
          Logger.error "Recieved Exception: (\#{client_id}, \#{inspect auth_info}): \#{inspect exception}"
          {:ok, st}
        end
      end
  """
  use GenEvent

  def start_link() do
    GenEvent.start_link(name: :skyline_errors)
  end

  def add_handler(handler, init) do
    GenEvent.add_handler(:skyline_errors, handler, init)
  end

end
