defmodule Skyline.Handler do
  # Functions to help dispatch messages.
  @moduledoc false
  require Logger

  alias Skyline.Client
  alias Skyline.Msg.{PublishReq, SubAck, PubAck, PubRel, PubRec, PubComp, PingResp, Subscribe, PingReq, Unsubscribe, UnsubAck}
  alias Skyline.Topic.Conn

  def handle_msg(%PingReq{}, %Client{sess_pid: sess_pid} ) do
    cast_msg(sess_pid,  PingResp.new())
    :ok
  end

  def handle_msg(%PubAck{msg_id: msg_id} = msg, %Client{client_id: client_id} ) do
    GenServer.cast({:global, {:qos_send, client_id, msg_id}}, {:next, msg})
    :ok
  end

  def handle_msg(%PubComp{msg_id: msg_id} = msg, %Client{client_id: client_id} ) do
    GenServer.cast({:global, {:qos_send, client_id, msg_id}}, {:next, msg})
    :ok
  end

  def handle_msg(%PubRec{msg_id: msg_id} = msg, %Client{client_id: client_id} ) do
    GenServer.cast({:global, {:qos_send, client_id, msg_id}}, {:next, msg})
    :ok
  end

  def handle_msg(%PubRel{msg_id: msg_id} = msg, %Client{client_id: client_id} ) do
    GenServer.cast({:global, {:qos_recv, client_id, msg_id}}, {:next, msg})
    :ok
  end

  def handle_msg(%PublishReq{topic: topic, qos: qos} = msg, %Client{app_config: config} = state) do
    case config.router.call(Conn.conn(topic, qos, msg, state, :publish), nil) do
      %Conn{} = ret_conn->
            Skyline.Topic.Utils.publish(ret_conn)
      :do_nothing ->
            :ok
      {:close_connection, _reason} = n ->
            n
      other ->
        raise "Expected Skyline.Conn, :do_nothing, or {:close_connection, reason}" <>
              " in return value for subsribe. Got #{inspect other}"
    end
    :ok
  end

  def handle_msg(%Subscribe{topics: topics} = msg, %Client{} = state) do
    handle_subscribe(topics, msg, [], state)
  end

  def handle_msg(%Unsubscribe{topics: topics, msg_id: msg_id}, %Client{sess_pid: sess_pid, client_id: client_id}) do
    :ok = GenServer.call(sess_pid, {:unsubscribe, topics})

    for topic <- topics do
      pid = GenServer.whereis({:global, {client_id, topic}})
      if is_pid(pid) && Process.alive?(pid) do
        :ok = GenServer.stop({client_id, topic})
      end
    end
    cast_msg(sess_pid, UnsubAck.new(msg_id))
    :ok
  end

  defp handle_subscribe([], %Subscribe{msg_id: msg_id}, qos_list, %Client{sess_pid: sess_pid}) do
    cast_msg(sess_pid, SubAck.new(Enum.reverse(qos_list), msg_id))
    :ok
  end
  defp handle_subscribe([{topic, qos}|topics], msg, qos_list, %Client{app_config: config} = state) do
    conn = Conn.conn(topic, qos, msg, state, :subscribe)
    case config.router.call(conn, nil) do
      %Conn{} = ret_conn ->
          qos = Skyline.Topic.Utils.subscribe(ret_conn)
          handle_subscribe(topics, msg, [qos | qos_list], state)
      {:topic_qos, qos} ->
          handle_subscribe(topics, msg, [qos | qos_list], state)
      {:close_connection, _reason} = n ->
          n
      other ->
        raise "Expected Skyline.Conn, {:topic_qos, qos}, or {:close_connection, reason}" <>
              " in return value for subsribe. Got #{inspect other}"
    end
  end

  defp cast_msg(sess_pid, msg) do
    GenServer.cast(sess_pid, {:msg, msg})
  end

end
