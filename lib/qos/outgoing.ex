defmodule Skyline.Qos.Outgoing.Qos0 do
  @moduledoc false

  # Outgoing QoS0.
  #
  # QoS0 is fire and forget so no observer process is needed.

  defstruct msg_queue: :queue.new

  def start(sess_pid, sub_id, _client_id, msg) do
    GenServer.cast(sess_pid, {:msg, msg})
    GenServer.cast(sub_id, {:finish_msg, msg.msg_id})
    {:ok, nil}
  end

end

defmodule Skyline.Qos.Outgoing.Qos1 do
  @moduledoc false

  # Outgoing QoS1.
  #
  # QoS1 requires an acknowledgement, so we much have a process to wait for the client's
  # acknowledgement or resend.

  defstruct msg: nil,
            sess_pid: nil,
            sub_id: nil

  use GenServer
  alias Skyline.Qos.Outgoing.Qos1
  alias Skyline.Msg.PubAck


  def start(sess_pid, sub_id, client_id, msg) do
    state = %Qos1{sess_pid: sess_pid, sub_id: sub_id, msg: msg}
    GenServer.start_link(__MODULE__, {msg, state}, name: {:global, {:qos_send, client_id, msg.msg_id}})
  end

  def init({msg, %Qos1{sess_pid: sess_pid} = state}) do
    GenServer.cast(sess_pid, {:msg, msg})
    {:ok, state, 15000}
  end

  def handle_cast({:next, %PubAck{}}, %Qos1{sub_id: sub_id, msg: msg} = state) do
    GenServer.cast(sub_id, {:finish_msg, msg.msg_id})
    {:stop, :normal, state}
  end

  def handle_info(:timeout, %Qos1{sess_pid: sess_pid, msg: msg} = state) do
    GenServer.cast(sess_pid, {:msg, msg})
    {:noreply, state, 15000}
  end
end
