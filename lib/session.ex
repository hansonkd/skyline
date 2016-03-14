defmodule Skiline.Session do
  use GenServer

  import Socket
  alias Skiline.Msg.{Connect, Subscribe, Encode, PublishDelivery}
  alias Skiline.Subscription

  defmodule State do
    defstruct socket: nil,
              client_id: nil,
              msg_id: 0,
              subs: %{},
              con_msg: nil,
              qos_queue: :queue.new

  end

  def start_link({socket, %Connect{client_id: client_id} = con}, _opts \\ []) do
    new_state =  %State{socket: socket, client_id: client_id, con_msg: con}
    GenServer.start_link(__MODULE__, new_state, name: {:global, {__MODULE__, client_id}})
  end

  def init(%State{client_id: client_id} = state) do
    :ets.insert(:session_msg_ids, {client_id, 0})
    {:ok, state}
  end

  def handle_call(:client_id, _from, %State{client_id: client_id} = state) do
    {:reply, {:ok, client_id}, state}
  end

  def handle_cast({:msg, msg}, %State{socket: socket} = state) do
    send_to_socket(socket, msg)
    {:noreply, state}
  end
  def handle_cast({:binary_msg, msg}, %State{socket: socket} = state) do
    send_binary_to_socket(socket, msg)
    {:noreply, state}
  end
  def send_to_socket(socket, msg) do
    send_binary_to_socket(socket, Encode.encode(msg))
  end
  defp send_binary_to_socket(socket, raw_msg) do
    ##IO.inspect({"raw", raw_msg, socket})
    Socket.Stream.send(socket, raw_msg)
  end
  defp increment_state(%State{msg_id: msg_id} = state) do
    %State{state | msg_id: msg_id + 1}
  end

end
