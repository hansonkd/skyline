defmodule Skyline.Subscription do
  @moduledoc false

  # Manages QoS and queueing the messages for a certain subscribe Topic.


  defstruct client_id: nil,
            sess_pid: nil,
            topic: "",
            qos: nil,
            current_msg: nil,
            msg_queue: :queue.new,
            qos_pid: nil,
            auth_info: nil



  use GenServer

  alias Skyline.Subscription
  alias Skyline.Msg.PublishReq

  use Skyline.Amnesia.Topic.TopicDatabase
  alias Skyline.Amnesia.Topic.TopicDatabase.StoredTopic


  def start_link(client_id, sess_pid, topic, qos, auth_info, _opts \\ []) do
    name = {client_id, topic}
    state = %Subscription{client_id: client_id, sess_pid: sess_pid, topic: topic, qos: qos, auth_info: auth_info}
    GenServer.start_link(__MODULE__, state, name: {:global, name})
  end

  def init(%Subscription{topic: topic} = state) do
    Skyline.Topic.Dispatcher.add_topic_subscription(topic, self)
    GenServer.cast(self, :check_for_stored_message)
    {:ok, state}
  end
  def handle_call({:reset, new_qos}, _from, state) do
    new_state = %{state | qos: new_qos, msg_queue: :queue.new}
    {:reply, {:ok, new_qos}, new_state}
  end
  def handle_call(:get_qos, _from, %Subscription{qos: qos} = state) do
    {:reply, {:ok, qos}, state}
  end
  def handle_cast(:check_for_stored_message, state) do
    check_for_stored_message(state)
    {:noreply, state}
  end
  def handle_cast({:publish, %PublishReq{} = msg}, %Subscription{client_id: client_id} = state) do
    :ets.update_counter(:session_msg_ids, client_id, 1)
    msg_id = :ets.update_counter(:session_msg_ids, client_id, 1)
    new_msg = PublishReq.convert_to_delivery(state.topic, state.qos, msg_id, false, msg)
    new_queue = :queue.in(new_msg, state.msg_queue)
    GenServer.cast(self, :process_queue)
    {:noreply, %{state | msg_queue: new_queue}}
  end
  def handle_cast(:process_queue, %Subscription{msg_queue: msg_queue, client_id: client_id, sess_pid: sess_pid, qos_pid: qos_pid} = state) do

    new_qos_pid = if not is_pid(qos_pid) || not Process.alive?(qos_pid) do
      case :queue.out(msg_queue) do
          {{:value, msg}, _new_queue} ->
            mod = qos_to_qos_mod(state.qos)
            {:ok, pid} = mod.start(sess_pid, self, client_id, msg)
            pid
          _ -> nil
        end
    else
      nil
    end

    {:noreply, %{state | qos_pid: new_qos_pid}}
  end
  def handle_cast({:finish_msg, msg_id}, %Subscription{msg_queue: msg_queue,
                                                       client_id: client_id,
                                                       auth_info: auth_info} = state) do
     new_queue = case :queue.out(msg_queue) do
        {{:value, msg}, trimmed} ->
          if msg.msg_id == msg_id do
            # Only "finish" if it was the right message.
            trimmed
          else
            Skyline.Events.error(client_id, auth_info, "Tried to finish message but expected #{msg.msg_id} but got #{msg_id}")
            msg_queue
          end
        _ -> msg_queue
      end
      if not :queue.is_empty(new_queue) do
        GenServer.cast(self, :process_queue)
      end
      {:noreply, %{state | msg_queue: new_queue}}
  end

  defp check_for_stored_message(state) do
    Amnesia.transaction do
      case StoredTopic.read(state.topic) do
        %StoredTopic{topic_id: t, message: m} ->
            pub_req = %PublishReq{topic: t, message: m}
            GenServer.cast(self, {:publish, pub_req})
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
