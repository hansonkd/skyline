defmodule Skiline.Qos.Outgoing.Qos0 do
  @moduledoc """
  Outgoing QoS0.

  QoS0 is fire and forget so no observer process is needed.
  """
  defstruct msg_queue: :queue.new

  def start(sess_pid, sub_id, cliend_id, msg) do
    GenServer.cast(sess_pid, {:msg, msg})
    GenServer.cast(sub_id, {:finish_msg, msg.msg_id})
    {:ok, nil}
  end

end

defmodule Skiline.Qos.Outgoing.Qos1 do
  @moduledoc """
  Outgoing QoS1.

  QoS1 requires an acknowledgement, so we much have a process to wait for the client's
  acknowledgement or resend.
  """
  defmodule Qos1State do
    defstruct msg: nil,
              sess_pid: nil,
              sub_id: nil
  end

  use GenServer
  alias Skiline.Msg.PubAck
  import Skiline.Router

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
        Skiline.Router.broadcast_msg(msg.topic, {:publish, msg})
      end
    end
  end
end

defmodule Skiline.Qos.Incoming.Qos0 do
  @moduledoc """
  Incoming QoS0.

  QoS0 is fire and forget so no observer process is needed.
  """

  use Incoming

  def start(sess_pid, cliend_id, msg) do
    #pid = spawn_link(fn() ->  bcast_msg(msg) end)
    bcast_msg(msg)
    {:ok, nil}
  end

end
defmodule Skiline.Qos.Incoming.Qos1 do
  @moduledoc """
  Incoming QoS1.

  QoS1 only requires an acknowledgement be sent, so no observer is needed.
  """

  use Incoming
  alias Skiline.Msg.PubAck

  def start(sess_pid, cliend_id, msg) do
    bcast_msg(msg)
    GenServer.cast(sess_pid, {:msg, PubAck.new(msg.msg_id)})
    {:ok, nil}
  end

end
