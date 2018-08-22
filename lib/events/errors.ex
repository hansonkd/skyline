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
#    GenEvent.start_link(name: :skyline_errors) #Deprecated

    GenServer.start_link(__MODULE__, :ok , name: :skyline_errors)
    #Starts a new GenServer passing three arguments:
    #1. The module where the server callbacks are implemented,
    #in this case __MODULE__, meaning the current module
    #2. The initialization arguments, in this case, the atom :ok
    #3. A list of options which can be used to specify things like the name of the server.
    #We can forward the list of options that we receive on start_link/1,
    #which defaults to an empty list. We will customize it later on
  end

  def add_handler(handler, init) do
    GenEvent.add_handler(:skyline_errors, handler, init)
  end

end
