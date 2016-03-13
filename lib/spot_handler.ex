defmodule Spotmq.Handler do
  alias Spotmq.Msg.{PublishReq, SubAck, PubAck, PingResp, Subscribe, PingReq, Unsubscribe, UnsubAck}
  alias Spotmq.Subscription

  use Spotmq.Persist.Topic.Database
  alias Spotmq.Persist.Topic.Database.{StoredTopic}

  def handle_msg(%Subscribe{msg_id: msg_id, topics: topics} = msg, sess_pid, _conn_msg) do
    {:ok, client_id} = GenServer.call(sess_pid, :client_id)

    qos_list = for {topic, qos} <- topics do
      case Subscription.start_link({client_id, sess_pid, topic, qos}) do
        {:ok, pid} ->
          {:ok, top_qos} = GenServer.call(pid, :get_qos)
          top_qos
        {:error, {:already_started, pid}} ->
           {:ok, top_qos} = GenServer.call(pid, {:reset, qos})
           top_qos
      end
    end
    cast_msg(sess_pid, SubAck.create(qos_list, msg_id), false)
  end
  def handle_msg(%Unsubscribe{topics: topics, msg_id: msg_id} = msg, sess_pid, _conn_msg) do
    :ok = GenServer.call(sess_pid, {:unsubscribe, topics})
    {:ok, client_id} = GenServer.call(sess_pid, :client_id)

    for topic <- topics do
      pid = GenServer.whereis({:global, {client_id, topic}})
      if is_pid(pid) && Process.alive?(pid) do
        :ok = GenServer.stop({client_id, topic})
      end
    end
    cast_msg(sess_pid, UnsubAck.create(msg_id), false)
  end

  def handle_msg(%PublishReq{} = msg, sess_pid, conn_msg) do
    ##IO.inspect("Sending Publish #{msg.topic}")
    if msg.retain do
      Amnesia.transaction do
        ##IO.inspect({"pub_req topic", StoredTopic.read(msg.topic)})
        st = case StoredTopic.read(msg.topic) do
          %StoredTopic{} = cst -> %{cst | message: msg.message}
          _ -> %StoredTopic{topic_id: msg.topic, message: msg.message}
        end
        st |> StoredTopic.write
      end
    end
    mod = qos_to_qos_mod(msg.qos)
    {:ok, _pid} = mod.start(sess_pid, conn_msg.client_id, msg)
  end
  def handle_msg(%PingReq{}, sess_pid, _conn_msg) do
    cast_msg(sess_pid,  PingResp.create(), false)
  end
  def handle_msg(%PubAck{msg_id: msg_id} = msg, sess_pid, conn_msg) do
    GenServer.cast({:global, {:qos_send, conn_msg.client_id, msg_id}}, {:next, msg})
  end
  defp cast_msg(sess_pid, msg, incr) do
    outgoing = if incr do
      {:ok, msg_id} = GenServer.call(sess_pid, :msg_id)
      %{msg | msg_id: msg_id}
    else
      msg
    end
    GenServer.cast(sess_pid, {:msg, outgoing})
  end
  def qos_to_qos_mod(qos) do
    alias Spotmq.Qos.Recipient.{Qos0, Qos1}
    case qos do
      :fire_and_forget -> Qos0
      :at_least_once -> Qos1
    end
  end
end
