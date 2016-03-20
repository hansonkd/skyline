defmodule Skyline.Handler do
  # Functions to help dispatch messages.
  @moduledoc false

  alias Skyline.Client
  alias Skyline.Msg.{PublishReq, SubAck, PubAck, PingResp, Subscribe, PingReq, Unsubscribe, UnsubAck}

  def handle_msg(%PingReq{}, sess_pid, _state) do
    cast_msg(sess_pid,  PingResp.new())
    :ok
  end

  def handle_msg(%PubAck{msg_id: msg_id} = msg, %Client{client_id: client_id} ) do
    GenServer.cast({:global, {:qos_send, client_id, msg_id}}, {:next, msg})
    :ok
  end

  def handle_msg(%PublishReq{topic: topic, qos: qos} = msg, %Client{app_config: config} = state) do
    config.router.call(Skyline.Topic.Conn.conn(topic, qos, msg, state, :publish), nil)
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
    conn = Skyline.Topic.Conn.conn(topic, qos, msg, state, :subscribe)
    case config.router.call(conn, nil) do
      {:ok, qos} ->
          handle_subscribe(topics, msg, [qos | qos_list], state)
      {:close_connection, _reason} = n ->
          n
    end
  end

  defp cast_msg(sess_pid, msg) do
    GenServer.cast(sess_pid, {:msg, msg})
  end

end
