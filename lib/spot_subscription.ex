defmodule Spotmq.Subscription do
  use GenServer
  defmodule State do
    defstruct client_id: nil,
              client_pid: nil,
              topic: "",
              qos: :fire_and_forget,
              current_msg: nil,
              msg_queue: :queue.new
  end

  import Amnesia
  use Spotmq.Persist.Topic.Database
  alias Spotmq.Persist.Topic.Database.{StoredTopic}
  alias Spotmq.Msg.{PublishReq}

  def start_link({client_id, client_pid, topic, qos}, _opts \\ []) do
    name = {client_id, topic}
    state = %State{client_id: client_id, client_pid: client_pid, topic: topic, qos: qos}
    ret = GenServer.start_link(__MODULE__, state, name: {:global, name})
    #IO.puts("Registered #{topic}")
    ret
  end

  def init(%State{topic: topic} = state) do
    #IO.puts("Registering #{topic}")
    :gproc.reg({:p, :l, {:topic, topic}})
    GenServer.cast(self, :check_for_stored_message)
    {:ok, state}
  end
  def handle_cast(:check_for_stored_message, state) do
    check_for_stored_message(state)
    {:noreply, state}
  end
  def handle_call({:reset, new_qos}, _from, state) do
    {:reply, {:ok, new_qos}, %{state | qos: new_qos, msg_queue: :queue.new}}
  end
  def handle_call(:get_qos, _from, %State{qos: qos} = state) do
    {:reply, {:ok, qos}, state}
  end

  def handle_cast({:publish, %PublishReq{} = msg}, %State{} = state) do
    #IO.inspect({"Cast publish", msg})
    {:ok, msg_id} = GenServer.call(state.client_pid, :msg_id)
    new_msg = PublishReq.convert_to_delivery(state.topic, state.qos, msg_id, false, msg)
    new_queue = :queue.in(new_msg, state.msg_queue)
    GenServer.cast(self, :process_queue)
    {:noreply, %{state | msg_queue: new_queue}}
  end
  def handle_cast(:process_queue, %State{current_msg: cur_msg, msg_queue: msg_queue} = state) do
    new_queue = case cur_msg do
      _ -> case :queue.out(msg_queue) do
              {{:value, msg}, new_queue} ->
                send_message(msg, state)
                if not :queue.is_empty(new_queue) do
                  GenServer.cast(self, :process_queue)
                end
                new_queue
              _ -> msg_queue
            end
    end
    {:noreply, %{state | msg_queue: new_queue}}
  end
  def handle_cast(other, state) do
    #IO.puts("not matched")
    #IO.inspect(other)
    {:noreply, state}
  end

  defp send_message(msg, %State{client_pid: client_pid}) do
    GenServer.cast(client_pid, {:msg, msg})
  end

  defp check_for_stored_message(state) do
    Amnesia.transaction do
      case StoredTopic.read(state.topic) do
        %StoredTopic{topic_id: t, message: m} ->
            pub_req = %PublishReq{topic: t, message: m}
            GenServer.cast(self, {:publish, pub_req})
        any -> #IO.inspect({"Mismatch Topics", any})
      end

    end
  end
end
