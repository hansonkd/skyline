defmodule Skyline.Events.Auth do
  @moduledoc """
  GenEvent server to handle connect and disconnect events.
  
  An app can hook into the Auth event system by calling `add_handler/2`
      defmodule Example.EventHandler do
        use GenEvent
        require Logger
        alias Skyline.Events.Auth
        def init() do
            Auth.add_handler(Example.EventHandler, nil)
        end
        def handle_event({:connect, {client_id, auth_info}}, st) do
          Logger.debug "New Connection: \#{client_id} \#{inspect auth_info}"
          {:ok, st}
        end
        def handle_event({:disconnect, {client_id, auth_info}}, st) do
          Logger.debug "Disconnected: \#{client_id} \#{inspect auth_info}"
          {:ok, st}
        end
      end
  """
  use GenEvent

  def start_link() do
    GenEvent.start_link(name: :skyline_auth_events)
  end

  def add_handler(handler, init) do
    GenEvent.add_handler(:skyline_auth_events, handler, init)
  end

end
