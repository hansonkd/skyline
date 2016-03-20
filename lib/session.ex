defmodule Skyline.Session do
  @moduledoc """

  Responsible for writing messages to a socket, starting up new subcriptions,
  and managing the session.
  """
  defstruct socket: nil,
            client_id: nil,
            auth_info: nil

  use GenServer

  import Socket
  alias Skyline.Session
  alias Skyline.Msg.{Connect, Subscribe, Encode, PublishDelivery}
  alias Skyline.Subscription

  @spec start_link(Skyline.socket, String.t, [key: any]) :: GenServer.on_start
  def start_link(socket, client_id, auth_info, _opts \\ []) do
    new_state =  %Session{socket: socket, client_id: client_id, auth_info: auth_info}
    GenServer.start_link(__MODULE__, new_state, name: {:global, {__MODULE__, client_id}})
  end

  def init(%Session{client_id: client_id} = state) do
    IO.puts("INSERT: #{inspect client_id}")
    :ets.insert(:session_msg_ids, {client_id, 0})
    {:ok, state}
  end

  def handle_cast({:msg, msg}, %Session{socket: socket} = state) do
    Skyline.Socket.send(socket, msg)
    {:noreply, state}
  end
end
