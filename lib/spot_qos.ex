defmodule Spotmq.Qos.Sender.Qos0 do
  defstruct msg_queue: :queue.new

  def start(sess_pid, sub_id, cliend_id, msg) do
    GenServer.cast(sess_pid, {:msg, msg})
    GenServer.cast(sub_id, {:finish_msg, msg.msg_id})
    {:ok, nil}
  end

end

defmodule Spotmq.Qos.Sender.Qos1 do
  defmodule Qos1State do
    defstruct msg: nil,
              sess_pid: nil,
              sub_id: nil
  end

  use GenServer
  alias Spotmq.Msg.PubAck

  def start(sess_pid, sub_id, client_id, msg) do
    state = %Qos1State{sess_pid: sess_pid, sub_id: sub_id, msg: msg}
    GenServer.start_link(__MODULE__, {msg, state}, name: {:global, {:qos_send, client_id, msg.msg_id}})
  end

  def init({msg, %Qos1State{sess_pid: sess_pid} = state}) do
    GenServer.cast(sess_pid, {:msg, msg})
    {:ok, state, 15000}
  end

  def handle_cast({:next, %PubAck{}}, %Qos1State{sub_id: sub_id, msg: msg} = state) do
    GenServer.cast(sub_id, {:finish_msg, msg.msg_id})
    {:stop, :normal, state}
  end

  def handle_info(:timeout, %Qos1State{sess_pid: sess_pid, msg: msg} = state) do
    GenServer.cast(sess_pid, {:msg, msg})
    {:noreply, state, 15000}
  end

end

defmodule Recipient do
  defmacro __using__(_opts) do
    quote do
      def bcast_msg(msg) do
        GenServer.cast({:via, :gproc, {:p, :l, {:topic, msg.topic}}}, {:publish, msg})
      end
    end
  end
end

defmodule Spotmq.Qos.Recipient.Qos0 do
  use Recipient

  def start(sess_pid, cliend_id, msg) do
    bcast_msg(msg)
    {:ok, self}
  end

end
defmodule Spotmq.Qos.Recipient.Qos1 do
  use Recipient
  alias Spotmq.Msg.PubAck

  def start(sess_pid, cliend_id, msg) do
    bcast_msg(msg)
    GenServer.cast(sess_pid, {:msg, PubAck.create(msg.msg_id)})
    {:ok, self}
  end

end
