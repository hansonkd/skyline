defmodule Skyline.Topic.Utils do
  @moduledoc """
  Utility functions for building Publish and Subscribe Controllers.
  """

  import Supervisor.Spec
  
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
                    private: %{socket: socket, client_id: client_id}}) do
    if retain do
      Amnesia.transaction do
        case StoredTopic.read(topic) do
          %StoredTopic{} = cst -> %{cst | message: body}
          _ -> %StoredTopic{topic_id: msg.topic, message: body}
        end |> StoredTopic.write
      end
    end
    mod = qos_to_qos_mod(qos)
    {:ok, _pid} = mod.start(socket, client_id, %{msg | topic: topic})
    :ok
  end

  @doc """
  Sets up a subsription to the topic and register with tree dispatcher.
  """
  @spec subscribe(Skyline.Conn.t) :: Skyline.qos_type
  def subscribe(%Conn{topic: topic,
                      qos: qos,
                      message: %Subscribe{},
                      auth_info: auth_info,
                      private: %{socket: socket, 
                                 client_id: client_id,
                                 supervisor_pid: supervisor_pid}}) do
      sub_worker = worker(Skyline.Subscription, [client_id, socket, topic, qos, auth_info], restart: :transient)
      IO.inspect sub_worker
      case Supervisor.start_child(supervisor_pid, sub_worker) do
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
