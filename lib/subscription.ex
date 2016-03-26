defmodule Skyline.Subscription do
  @moduledoc false

  # Manages QoS and queueing the messages for a certain subscribe Topic.

  defstruct [:client_id, :socket, :topic, :auth_info, :name, :qos]

  alias Skyline.Subscription
  alias Skyline.Msg.PublishReq

  use Skyline.Amnesia.Topic.TopicDatabase
  alias Skyline.Amnesia.Topic.TopicDatabase.StoredTopic


  def start_link(client_id, socket, topic, qos, auth_info, _opts \\ []) do
    name = String.to_atom(client_id <> "___" <> topic)
    state = %Subscription{client_id: client_id, name: name, socket: socket, topic: topic, qos: qos, auth_info: auth_info}
    pid = spawn_link(fn() -> init(state) end)
    {:ok, pid}
  end

  def init(%Subscription{name: name, topic: topic} = state) do
    Skyline.Topic.Dispatcher.add_topic_subscription(topic, {self, name})
    check_for_stored_message(state)
  end

  def handle_info(:timeout, state) do
    process_queue(state)
    {:noreply, state, 500}
  end

  def process_queue(%Subscription{name: name, client_id: client_id, socket: socket} = state) do
    receive do
      {:publish, msg} ->
        :ets.update_counter(:session_msg_ids, client_id, 1)
        msg_id = :ets.update_counter(:session_msg_ids, client_id, 1)
        new_msg = PublishReq.convert_to_delivery(state.topic, state.qos, msg_id, false, msg)
        mod = qos_to_qos_mod(state.qos)
        {:ok, pid} = mod.start(socket, self, client_id, new_msg)
    end
    process_queue(state)
  end

  defp qflush() do
    receive do
        _ -> qflush()
    after 0 ->
        :ok
    end
  end

  defp check_for_stored_message(state) do
    Amnesia.transaction do
      case StoredTopic.read(state.topic) do
        %StoredTopic{topic_id: t, message: m} ->
            pub_req = %PublishReq{topic: t, message: m}
            :ets_buffer.write(state.name, {:publish, pub_req})
        nil -> :_
      end
    end
    process_queue(state)
  end

  def qos_to_qos_mod(qos) do
    case qos do
      :fire_and_forget -> Skyline.Qos.Outgoing.Qos0
      :at_least_once -> Skyline.Qos.Outgoing.Qos1
      :exactly_once -> Skyline.Qos.Outgoing.Qos2
    end
  end
end
