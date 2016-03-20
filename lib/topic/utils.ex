defmodule Skyline.Topic.Utils do

  alias Skyline.Client
  alias Skyline.Topic.PublishHandler
  alias Skyline.Msg.{PublishReq, SubAck, Subscribe, Unsubscribe, UnsubAck}
  alias Skyline.Subscription

  use Skyline.Amnesia.Topic.Database
  alias Skyline.Amnesia.Topic.Database.StoredTopic

  @doc """
  Deliver the message to all subscribers, retain if necassary.
  """
  @spec publish(PublishReq.t, Client.t) :: Client.t
  def publish(%PublishReq{topic: topic} = msg, %Client{sess_pid: sess_pid, client_id: client_id, app: app} = state) do
    if msg.retain do
      Amnesia.transaction do
        case StoredTopic.read(msg.topic) do
          %StoredTopic{} = cst -> %{cst | message: msg.message}
          _ -> %StoredTopic{topic_id: msg.topic, message: msg.message}
        end |> StoredTopic.write
      end
    end
    mod = qos_to_qos_mod(msg.qos)
    {:ok, _pid} = mod.start(sess_pid, client_id, msg)
    :ok
  end

  @doc """
  Sets up a subsription to the topic and register with tree dispatcher.
  """
  @spec subscribe(Subscribe.t, Client.t) :: Client.t
  def subscribe(%Subscribe{msg_id: msg_id, topics: topics} = msg, %Client{sess_pid: sess_pid, client_id: client_id} = state) do

    qos_list = for {topic, qos} <- topics do
      case Subscription.start_link({client_id, sess_pid, topic, qos}) do
        {:ok, pid} -> qos
        {:error, {:already_started, pid}} ->
           {:ok, top_qos} = GenServer.call(pid, {:reset, qos})
           top_qos
      end
    end
    cast_msg(sess_pid, SubAck.new(qos_list, msg_id))
    state
  end

  defp cast_msg(sess_pid, msg) do
    GenServer.cast(sess_pid, {:msg, msg})
  end

  defp qos_to_qos_mod(qos) do
    case qos do
      :fire_and_forget -> Skyline.Qos.Incoming.Qos0
      :at_least_once -> Skyline.Qos.Incoming.Qos1
    end
  end
end
