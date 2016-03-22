defmodule Skyline.Handler do
  # Functions to help dispatch messages.
  @moduledoc false
  require Logger

  alias Skyline.Socket
  alias Skyline.Client
  alias Skyline.Msg.{PublishReq, SubAck, PubAck, PubRel, PubRec, PubComp,
                     PingResp, Subscribe, PingReq, Unsubscribe, UnsubAck}
  alias Skyline.Topic.Conn
  alias Skyline.AppConfig

  use Skyline.Amnesia.Session.SessionDatabase

  def handle_msg(%PingReq{}, %Client{socket: socket} = state) do
    Socket.send(socket, PingResp.new())
    state
  end

  def handle_msg(%PubAck{msg_id: msg_id} = msg, %Client{client_id: client_id} = state) do
    GenServer.cast({:global, {:qos_send, client_id, msg_id}}, {:next, msg})
    state
  end

  def handle_msg(%PubComp{msg_id: msg_id} = msg, %Client{client_id: client_id} = state) do
    GenServer.cast({:global, {:qos_send, client_id, msg_id}}, {:next, msg})
    state
  end

  def handle_msg(%PubRec{msg_id: msg_id} = msg, %Client{client_id: client_id} = state) do
    GenServer.cast({:global, {:qos_send, client_id, msg_id}}, {:next, msg})
    state
  end

  def handle_msg(%PubRel{msg_id: msg_id} = msg, %Client{client_id: client_id} = state) do
    GenServer.cast({:global, {:qos_recv, client_id, msg_id}}, {:next, msg})
    state
  end

  def handle_msg(%PublishReq{topic: topic, qos: qos} = msg,
                 %Client{app_config: %AppConfig{router_module: router_mod, router_opts: router_opts}} = state) do
    case router_mod.call(Conn.conn(topic, qos, msg, :publish, state), router_opts) do
      %Conn{auth_info: auth_info} = ret_conn->
            Skyline.Topic.Utils.publish(ret_conn)
            %{state | auth_info: auth_info}
      :do_nothing ->
            state
      {:close_connection, _reason} = n ->
            n
      other ->
        raise "Expected Skyline.Conn, :do_nothing, or {:close_connection, reason}" <>
              " in return value for subsribe. Got #{inspect other}"
    end
  end

  def handle_msg(%Subscribe{topics: topics, msg_id: msg_id} = msg,
                 %Client{socket: socket, client_id: client_id, persistent_session: persist} = state) do
    case handle_subscribe(topics, msg, [], state) do
      {qos_list, new_state} ->
          Socket.send(socket, SubAck.new(qos_list, msg_id))
          if persist do
            save_topics(client_id, topics)
          end
          new_state
      other -> other
    end

  end

  def handle_msg(%Unsubscribe{topics: topics, msg_id: msg_id}, %Client{socket: socket, client_id: client_id} = state) do
    #:ok = GenServer.call(sess_pid, {:unsubscribe, topics})

    for topic <- topics do
      pid = GenServer.whereis({:global, {client_id, topic}})
      if is_pid(pid) && Process.alive?(pid) do
        :ok = GenServer.stop({client_id, topic})
      end
    end
    Socket.send(socket, UnsubAck.new(msg_id))
    state
  end

  def handle_subscribe([], %Subscribe{}, qos_list, %Client{} = state) do
    {Enum.reverse(qos_list), state}
  end
  def handle_subscribe([{topic, qos}|topics], msg, qos_list,
                        %Client{app_config: %AppConfig{router_module: router_mod,
                                                       router_opts: router_opts}} = state) do
    conn = Conn.conn(topic, qos, msg, :subscribe, state)
    case router_mod.call(conn, router_opts) do
      %Conn{auth_info: auth_info} = ret_conn ->
          qos = Skyline.Topic.Utils.subscribe(ret_conn)
          handle_subscribe(topics, msg, [qos | qos_list], %{state | auth_info: auth_info})
      {:topic_qos, qos} ->
          handle_subscribe(topics, msg, [qos | qos_list], state)
      {:close_connection, _reason} = n ->
          n
      other ->
        raise "Expected Skyline.Conn, {:topic_qos, qos}, or {:close_connection, reason}" <>
              " in return value for subsribe. Got #{inspect other}"
    end
  end

  defp save_topics(client_id, new_topics) do
    Amnesia.transaction do
      case StoredSession.read(client_id) do
        %StoredSession{topics: topics} = s ->
          %{s | topics: Enum.uniq(topics ++ new_topics)} |> StoredSession.write
      end
    end
  end
end
