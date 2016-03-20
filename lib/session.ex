defmodule Skyline.Session do
  @moduledoc false

  # Responsible for writing messages to a socket, starting up new subcriptions,
  # and managing the session.

  defstruct socket: nil,
            client_id: nil,
            auth_info: nil

  use GenServer
  require Logger

  alias Skyline.Session

  @spec start_link(Skyline.socket, String.t, [key: any]) :: GenServer.on_start
  def start_link(socket, client_id, auth_info, _opts \\ []) do
    new_state =  %Session{socket: socket, client_id: client_id, auth_info: auth_info}
    GenServer.start_link(__MODULE__, new_state, name: {:global, {__MODULE__, client_id}})
  end

  def init(%Session{client_id: client_id} = state) do
    :ets.insert(:session_msg_ids, {client_id, 0})
    {:ok, state}
  end

  def handle_cast({:msg, msg}, %Session{socket: socket, client_id: client_id} = state) do
    Logger.debug "Sending message #{inspect msg} to #{inspect client_id}"
    Skyline.Socket.send(socket, msg)
    {:noreply, state}
  end
end
