defmodule Skyline.Subscription do
  @moduledoc false

  # Manages QoS and queueing the messages for a certain subscribe Topic.


  defstruct client_id: nil,
            socket: nil,
            topic: "",
            qos: nil,
            current_msg: nil,
            msg_queue: :queue.new,
            qos_pid: nil,
            auth_info: nil,
            name: nil



  use GenServer

  alias Skyline.Subscription
  alias Skyline.Msg.PublishReq

  use Skyline.Amnesia.Topic.TopicDatabase
  alias Skyline.Amnesia.Topic.TopicDatabase.StoredTopic


  def start_link(client_id, socket, topic, qos, auth_info, _opts \\ []) do
    name = String.to_atom(client_id <> "___" <> topic)
    state = %Subscription{client_id: client_id, name: name, socket: socket, topic: topic, qos: qos, auth_info: auth_info}
    GenServer.start_link(__MODULE__, state)
  end

  def init(%Subscription{name: name, topic: topic} = state) do
    :ets_buffer.create(name, :fifo)
    Skyline.Topic.Dispatcher.add_topic_subscription(topic, name)
    GenServer.cast(self, :check_for_stored_message)
    {:ok, state, 500}
  end
  def handle_call({:reset, new_qos}, _from, state) do
    new_state = %{state | qos: new_qos, msg_queue: :queue.new}
    check_for_stored_message(new_state)
    {:reply, {:ok, new_qos}, new_state}
  end

  def handle_cast(:check_for_stored_message, state) do
    check_for_stored_message(state)
    {:noreply, state, 500}
  end

  def handle_cast(:process_queue, state) do

    process_queue(state)

    {:noreply, state, 500}
  end

  def handle_info(:timeout, state) do
    process_queue(state)
    {:noreply, state, 500}
  end

  def process_queue(%Subscription{name: name, client_id: client_id, socket: socket, qos_pid: qos_pid} = state) do
    case :ets_buffer.read(name) do
      [{:publish, msg}] ->
        :ets.update_counter(:session_msg_ids, client_id, 1)
        msg_id = :ets.update_counter(:session_msg_ids, client_id, 1)
        new_msg = PublishReq.convert_to_delivery(state.topic, state.qos, msg_id, false, msg)
        mod = qos_to_qos_mod(state.qos)
        {:ok, pid} = mod.start(socket, self, client_id, new_msg)
        if :ets_buffer.num_entries(name) > 0 do
          process_queue(state)
        end
      _ -> :_
    end
  end


  defp check_for_stored_message(state) do
    Amnesia.transaction do
      case StoredTopic.read(state.topic) do
        %StoredTopic{topic_id: t, message: m} ->
            pub_req = %PublishReq{topic: t, message: m}
            :ets_buffer.write(state.name, {:publish, pub_req})
            GenServer.cast(self, :process_queue)
        nil -> nil
      end
    end
  end

  def qos_to_qos_mod(qos) do
    case qos do
      :fire_and_forget -> Skyline.Qos.Outgoing.Qos0
      :at_least_once -> Skyline.Qos.Outgoing.Qos1
      :exactly_once -> Skyline.Qos.Outgoing.Qos2
    end
  end
end
