defmodule Spotmq.Qos.Outgoing.Qos0 do
  defstruct msg_queue: :queue.new

  def start(sess_pid, sub_id, cliend_id, msg) do
    GenServer.cast(sess_pid, {:msg, msg})
    GenServer.cast(sub_id, {:finish_msg, msg.msg_id})
    {:ok, nil}
  end

end

defmodule Spotmq.Qos.Outgoing.Qos1 do
  defmodule Qos1State do
    defstruct msg: nil,
              sess_pid: nil,
              sub_id: nil
  end

  use GenServer
  alias Spotmq.Msg.PubAck
  import Spotmq.Router

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

defmodule Incoming do
  defmacro __using__(_opts) do
    quote do
      def bcast_msg(msg) do
        #GenServer.cast({:via, :gproc, {:p, :l, {:topic, }}}, )
        Spotmq.Router.broadcast_msg(msg.topic, {:publish, msg})
      end
    end
  end
end

defmodule Spotmq.Qos.Incoming.Qos0 do
  use Incoming

  def start(sess_pid, cliend_id, msg) do
    #pid = spawn_link(fn() ->  bcast_msg(msg) end)
    bcast_msg(msg)
    {:ok, nil}
  end

end
defmodule Spotmq.Qos.Incoming.Qos1 do
  use Incoming
  alias Spotmq.Msg.PubAck

  def start(sess_pid, cliend_id, msg) do
    pid = spawn_link(
      fn() ->
        bcast_msg(msg)
        GenServer.cast(sess_pid, {:msg, PubAck.create(msg.msg_id)})
      end
    )
    #bcast_msg(msg)
    {:ok, pid}
  end

end
