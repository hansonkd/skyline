defmodule Skyline.Events.Incoming do
  @moduledoc """
  GenEvent server to hook into *all* incoming messages direct from the client 
  (before pipelines, controllers, etc).
  
  An app can hook into the Incoming event system by calling `add_handler/2`
      defmodule Example.EventHandler do
        use GenEvent
        require Logger
        alias Skyline.Events.Incoming
        def init() do
            Incoming.add_handler(Example.EventHandler, nil)
        end
        def handle_event({:incoming_message, {client_id, auth_info, message}}, st) do
          Logger.debug "Accepted message (\#{client_id}, \#{inspect auth_info}): \#{inspect message}"
          {:ok, st}
        end
      end
  """
  use GenEvent

  def start_link() do
#    GenEvent.start_link(name: :skyline_incoming) #Deprecated

    GenServer.start_link(__MODULE__, :ok , name: :skyline_incoming)
    #Starts a new GenServer passing three arguments:
    #1. The module where the server callbacks are implemented,
    #in this case __MODULE__, meaning the current module
    #2. The initialization arguments, in this case, the atom :ok
    #3. A list of options which can be used to specify things like the name of the server.
    #We can forward the list of options that we receive on start_link/1,
    #which defaults to an empty list. We will customize it later on
  end


  def add_handler(handler, init) do
    GenEvent.add_handler(:skyline_incoming, handler, init)
  end

end
