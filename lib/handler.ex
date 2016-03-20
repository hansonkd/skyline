defmodule Skyline.Handler do
  @moduledoc """
  Functions to help dispatch messages.
  """
  alias Skyline.Client
  alias Skyline.Msg.{PublishReq, SubAck, PubAck, PingResp, Subscribe, PingReq, Unsubscribe, UnsubAck}
  alias Skyline.Subscription


  def handle_msg(%Subscribe{msg_id: msg_id, topics: topics} = msg, %Client{sess_pid: sess_pid, app_config: config} = state) do
    IO.puts("Subscribe 1: #{inspect msg}")
    qos_list = for {topic, qos} <- topics do
      IO.puts("Topic : #{inspect topic}")
      ret = config.router.call(Skyline.Topic.Conn.conn(topic, qos, msg, state, :subscribe), nil)
      IO.puts("Handle Sub Ret: #{inspect ret}")
      qos
    end
    IO.puts("Subscribe 2: #{inspect msg}")
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

  def handle_msg(%PublishReq{topic: topic, qos: qos} = msg, %Client{app_config: config} = state) do
    ##IO.inspect("Sending Publish #{msg.topic}")
    config.router.call(Skyline.Topic.Conn.conn(topic, qos, msg, state, :publish), nil)
    state
  end

  def handle_msg(%PingReq{}, sess_pid, _state) do
    cast_msg(sess_pid,  PingResp.new())
  end
  def handle_msg(%PubAck{msg_id: msg_id} = msg, %Client{client_id: client_id} = state) do
    GenServer.cast({:global, {:qos_send, client_id, msg_id}}, {:next, msg})
  end
  defp cast_msg(sess_pid, msg) do
    IO.puts("cast msg: #{inspect msg}")
    GenServer.cast(sess_pid, {:msg, msg})
  end

end
