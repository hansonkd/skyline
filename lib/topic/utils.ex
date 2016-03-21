defmodule Skyline.Topic.Utils do
  @moduledoc """
  Utility functions for building Publish and Subscribe Controllers.
  """

  alias Skyline.Client
  alias Skyline.Msg.{PublishReq, Subscribe}
  alias Skyline.Subscription
  alias Skyline.Topic.Conn

  use Skyline.Amnesia.Topic.TopicDatabase
  alias Skyline.Amnesia.Topic.TopicDatabase.StoredTopic

  @doc """
  Deliver the message to all subscribers, retain if necassary.
  """
  @spec publish(Skyline.Conn.t) :: :ok
  def publish(%Conn{message: %PublishReq{message: body,
                                        retain: retain} = msg,
                    topic: topic,
                    qos: qos,
                    client: %Client{sess_pid: sess_pid,
                                    client_id: client_id}}) do
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
  @spec subscribe(Skyline.Conn.t) :: Skyline.qos_type
  def subscribe(%Conn{topic: topic,
                      qos: qos,
                      message: %Subscribe{},
                      client: %Client{sess_pid: sess_pid,
                                      client_id: client_id,
                                      auth_info: auth_info}}) do
      case Subscription.start_link(client_id, sess_pid, topic, qos, auth_info) do
        {:ok, _pid} -> qos
        {:error, {:already_started, pid}} ->
           {:ok, top_qos} = GenServer.call(pid, {:reset, qos})
           top_qos
      end
  end

  defp qos_to_qos_mod(qos) do
    case qos do
      :fire_and_forget -> Skyline.Qos.Incoming.Qos0
      :at_least_once -> Skyline.Qos.Incoming.Qos1
      :exactly_once -> Skyline.Qos.Incoming.Qos2
    end
  end
end
