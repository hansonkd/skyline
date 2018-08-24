defmodule Skyline.Events.Outgoing do
  @moduledoc """
  GenEvent server to hook into *all* outbound messages as they will appear to the client.
  
  An app can hook into the Outgoing event system by calling `add_handler/2`
      defmodule Example.EventHandler do
        use GenEvent
        require Logger
        alias Skyline.Events.Outgoing
        def init() do
            Outgoing.add_handler(Example.EventHandler, nil)
        end
        def handle_event({:outgoing_message, {client_id, auth_info, message}}, st) do
          Logger.debug "Sending message (\#{client_id}, \#{inspect auth_info}): \#{inspect message}"
          {:ok, st}
        end
      end
  """
  use GenEvent

  def start_link() do
    GenEvent.start_link(name: :skyline_outgoing)
  end


  def add_handler(handler, init) do
    GenEvent.add_handler(:skyline_outgoing, handler, init)
  end

end
