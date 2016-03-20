defmodule Skyline.Topic.Utils do

  alias Skyline.Client
  alias Skyline.Topic.PublishHandler
  alias Skyline.Msg.{PublishReq, SubAck, Subscribe, Unsubscribe, UnsubAck}
  alias Skyline.Subscription

  use Skyline.Amnesia.Topic.TopicDatabase
  alias Skyline.Amnesia.Topic.TopicDatabase.StoredTopic

  @doc """
  Deliver the message to all subscribers, retain if necassary.
  """
  @spec publish(String.t, Skyline.qos_type, PublishReq.t, Client.t) :: Client.t
  def publish(topic, qos, %PublishReq{message: body, retain: retain} = msg, %Client{sess_pid: sess_pid, client_id: client_id} = state) do
    if retain do
      Amnesia.transaction do
        case StoredTopic.read(topic) do
          %StoredTopic{} = cst -> %{cst | message: body}
          _ -> %StoredTopic{topic_id: msg.topic, message: body}
        end |> StoredTopic.write
      end
    end
    mod = qos_to_qos_mod(qos)
    {:ok, _pid} = mod.start(sess_pid, client_id, msg)
    :ok
  end

  @doc """
  Sets up a subsription to the topic and register with tree dispatcher.
  """
  @spec subscribe(String.t, Skyline.qos_type, Subscribe.t, Client.t) :: Client.t
  def subscribe(topic, qos, %Subscribe{msg_id: msg_id} = msg, %Client{sess_pid: sess_pid, client_id: client_id} = state) do
      case Subscription.start_link({client_id, sess_pid, topic, qos}) do
        {:ok, pid} -> qos
        {:error, {:already_started, pid}} ->
           {:ok, top_qos} = GenServer.call(pid, {:reset, qos})
           top_qos
      end
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
