defmodule Spotmq.Subscription do

  defmodule State do
    defstruct client_id: nil,
              sess_pid: nil,
              topic: "",
              qos: nil,
              current_msg: nil,
              msg_queue: :queue.new,
              qos_pid: nil
  end

  import Amnesia
  use GenServer
  use Spotmq.Persist.Topic.Database
  import Spotmq.Router
  alias Spotmq.Persist.Topic.Database.{StoredTopic}
  alias Spotmq.Msg.{PublishReq}
  alias Spotmq.Qos.Outgoing.{Qos0, Qos1}

  def start_link({client_id, sess_pid, topic, qos}, _opts \\ []) do
    name = {client_id, topic}
    state = %State{client_id: client_id, sess_pid: sess_pid, topic: topic, qos: qos}
    ret = GenServer.start_link(__MODULE__, state, name: {:global, name})
    #IO.puts("Registered #{topic}")
    ret
  end

  def init(%State{topic: topic} = state) do
    #IO.puts("Registering #{topic}")
    #:gproc.reg({:p, :l, {:topic, topic}})
    Spotmq.Router.add_topic_subscription(topic, self)
    GenServer.cast(self, :check_for_stored_message)
    {:ok, state}
  end

  def handle_call({:reset, new_qos}, _from, state) do
    new_state = %{state | qos: new_qos, msg_queue: :queue.new}
    {:reply, {:ok, new_qos}, new_state}
  end
  def handle_call(:get_qos, _from, %State{qos: qos} = state) do
    {:reply, {:ok, qos}, state}
  end
  def handle_cast(:check_for_stored_message, state) do
    check_for_stored_message(state)
    {:noreply, state}
  end
  def handle_cast({:publish, %PublishReq{} = msg}, %State{client_id: client_id} = state) do
    #IO.inspect({"Cast publish", msg})
    msg_id = :ets.update_counter(:session_msg_ids, client_id, 1)
    new_msg = PublishReq.convert_to_delivery(state.topic, state.qos, msg_id, false, msg)
    new_queue = :queue.in(new_msg, state.msg_queue)
    GenServer.cast(self, :process_queue)
    {:noreply, %{state | msg_queue: new_queue}}
  end
  def handle_cast(:process_queue, %State{msg_queue: msg_queue, qos_pid: qos_pid} = state) do

    new_qos_pid = if not is_pid(qos_pid) || not Process.alive?(qos_pid) do
      case :queue.out(msg_queue) do
          {{:value, msg}, _new_queue} ->
            mod = qos_to_qos_mod(state.qos)
            {:ok, pid} = mod.start(state.sess_pid, self, state.client_id, msg)
            pid
          _ -> nil
        end
    else
      nil
    end

    {:noreply, %{state | qos_pid: new_qos_pid}}
  end
  def handle_cast({:finish_msg, msg_id}, %State{msg_queue: msg_queue} = state) do
     new_queue = case :queue.out(msg_queue) do
        {{:value, msg}, new_queue} ->
          if msg.msg_id == msg_id do
            # Only "finish" if it was the right message.
            new_queue
          else
            msg_queue
          end
        _ -> msg_queue
      end
      if not :queue.is_empty(new_queue) do
        GenServer.cast(self, :process_queue)
      end
      {:noreply, %{state | msg_queue: new_queue}}
  end

  def handle_cast(other, state) do
    IO.puts("not matched")
    IO.inspect({other, state})
    {:noreply, state}
  end

  defp send_message(msg, %State{sess_pid: sess_pid}) do
    GenServer.cast(sess_pid, {:msg, msg})
  end

  defp check_for_stored_message(state) do
    Amnesia.transaction do
      case StoredTopic.read(state.topic) do
        %StoredTopic{topic_id: t, message: m} ->
            pub_req = %PublishReq{topic: t, message: m}
            GenServer.cast(self, {:publish, pub_req})
        nil -> nil
      end
    end
  end

  def qos_to_qos_mod(qos) do
    case qos do
      :fire_and_forget -> Qos0
      :at_least_once -> Qos1
    end
  end
end
