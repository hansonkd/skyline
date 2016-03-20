defmodule Skyline.Handler do
  @moduledoc """
  Functions to help dispatch messages.
  """
  alias Skyline.Client
  alias Skyline.Msg.{PublishReq, SubAck, PubAck, PingResp, Subscribe, PingReq, Unsubscribe, UnsubAck}
  alias Skyline.Subscription


  def handle_msg(%Subscribe{msg_id: msg_id, topics: topics} = msg, %Client{sess_pid: sess_pid, client_id: client_id} = state) do

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
  def handle_msg(%Unsubscribe{topics: topics, msg_id: msg_id} = msg, %Client{sess_pid: sess_pid, client_id: client_id} = state) do
    :ok = GenServer.call(sess_pid, {:unsubscribe, topics})

    for topic <- topics do
      pid = GenServer.whereis({:global, {client_id, topic}})
      if is_pid(pid) && Process.alive?(pid) do
        :ok = GenServer.stop({client_id, topic})
      end
    end
    cast_msg(sess_pid, UnsubAck.new(msg_id))
    state
  end

  def handle_msg(%PublishReq{} = msg, state) do
    ##IO.inspect("Sending Publish #{msg.topic}")
    {:ok, new_state} = Skyline.Topic.PublishDispatcher.publish(msg, state)
    new_state
  end

  def handle_msg(%PingReq{}, sess_pid, _state) do
    cast_msg(sess_pid,  PingResp.new())
  end
  def handle_msg(%PubAck{msg_id: msg_id} = msg, %Client{client_id: client_id} = state) do
    GenServer.cast({:global, {:qos_send, client_id, msg_id}}, {:next, msg})
  end
  defp cast_msg(sess_pid, msg) do
    GenServer.cast(sess_pid, {:msg, msg})
  end

end
