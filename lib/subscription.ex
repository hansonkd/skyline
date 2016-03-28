defmodule Skyline.Subscription do
  @moduledoc false

  # Manages QoS and queueing the messages for a certain subscribe Topic.

  defstruct [:client_id, :socket, :topic, :auth_info,
             :name, :qos, :msg_queue, :qos_pool, :qos_all]

  alias Skyline.Subscription
  alias Skyline.Msg.PublishReq

  use Skyline.Amnesia.Topic.TopicDatabase
  alias Skyline.Amnesia.Topic.TopicDatabase.StoredTopic
  require Logger

  def start_link(client_id, socket, topic, qos, auth_info, _opts \\ []) do
    name = String.to_atom(client_id <> "___" <> topic)
    state = %Subscription{
      client_id: client_id,
      name: name,
      socket: socket,
      topic: topic,
      qos: qos,
      auth_info: auth_info,
      msg_queue: :queue.new,
      qos_pool: [],
      qos_all: []
    }
    pid = spawn_link(fn() -> init(state) end)
    {:ok, pid}
  end

  def init(%Subscription{name: name, topic: topic} = state) do
    Skyline.Topic.Dispatcher.add_topic_subscription(topic, {self, name})
    check_for_stored_message(state)
  end

  def process_queue(%Subscription{msg_queue: msg_queue, qos_pool: pool} = state) do
    case :queue.out(msg_queue) do
        {{:value, item}, new_queue} ->
            publish_key(item, %{state | msg_queue: new_queue})
        _ ->
            listen(state)
    end
  end

  def listen(%Subscription{msg_queue: msg_queue, qos_pool: pool} = state) do
    receive do
      {:publish_key, msg_key} ->
          if (Enum.count(pool) > 1) and (:queue.is_empty msg_queue) do
              publish_key(msg_key, state)
          else
             process_queue(%{state | msg_queue:  :queue.in(msg_key, msg_queue)})
          end
      {:finished, pid} ->
          process_queue(%{state | qos_pool: [ pid | pool ]})
      {:terminated, pid} ->
          process_queue(%{state | qos_pool: Enum.delete(pid, pool)})
    end
  end

  defp publish_key(msg_key, state) do

    case ConCache.get(:msg_cache, msg_key) do
      %PublishReq{} = msg ->
          publish_msg(msg, state)
      n ->
        Logger.error "Could not find pubreq #{inspect msg_key}, got #{inspect n}"
        process_queue(state)
    end
  end

  defp publish_msg(msg, %Subscription{name: name, client_id: client_id, socket: socket} = state) do
    msg_id = :ets.update_counter(:session_msg_ids, client_id, 1)
    new_msg = PublishReq.convert_to_delivery(state.topic, state.qos, msg_id, false, msg)
    mod = qos_to_qos_mod(state.qos)
    case state.qos_pool do
      [h | t] ->
        send(h, {:send_msg, new_msg})
        process_queue(%{state | qos_pool: t})
      _ ->
        if Enum.count(state.qos_all) < 20 do
           {:ok, new_pid} = mod.start(socket, self, client_id, new_msg)
           process_queue(%{state | qos_all: [new_pid | state.qos_all]})
        else
           listen(state)
        end
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
