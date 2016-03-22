defmodule Incoming do
  @moduledoc false

  defmacro __using__(_opts) do
    quote do
      def timeout do
        Application.get_env(:skyline, :qos_timeout, 15000)
      end
      def bcast_msg(msg) do
        Skyline.Topic.Dispatcher.broadcast_msg(msg.topic, {:publish, msg})
      end
    end
  end
end

defmodule Skyline.Qos.Incoming.Qos0 do
  @moduledoc false

  # Incoming QoS0.
  #
  # QoS0 is fire and forget so no observer process is needed.

  use Incoming

  def start(_sess_pid, _client_id, msg) do
    #pid = spawn_link(fn() ->  bcast_msg(msg) end)
    bcast_msg(msg)
    {:ok, nil}
  end

end
defmodule Skyline.Qos.Incoming.Qos1 do
  @moduledoc false

  # Incoming QoS1.
  #
  # QoS1 only requires an acknowledgement be sent, so no observer is needed.

  use Incoming
  alias Skyline.Msg.PubAck

  def start(sess_pid, _client_id, msg) do
    bcast_msg(msg)
    GenServer.cast(sess_pid, {:msg, PubAck.new(msg.msg_id)})
    {:ok, nil}
  end

end
defmodule Skyline.Qos.Incoming.Qos2 do
  @moduledoc false

  # Incoming QoS2.
  #
  # QoS2 requires a response message so we need an observer process
  defstruct msg: nil,
            sess_pid: nil,
            client_id: nil

  use GenServer
  use Incoming

  require Logger

  alias Skyline.Msg.{PubRec, PubRel, PubComp}
  alias Skyline.Qos.Incoming.Qos2

  def start(sess_pid, client_id, msg) do
    state = %Qos2{sess_pid: sess_pid, client_id: client_id,  msg: msg}
    GenServer.start_link(__MODULE__, {msg, state}, name: {:global, {:qos_recv, client_id, msg.msg_id}})
  end

  def init({msg, %Qos2{sess_pid: sess_pid} = state}) do
    GenServer.cast(sess_pid, {:msg, PubRec.new(msg.msg_id)})
    {:ok, state, timeout}
  end

  def handle_cast({:next, %PubRel{}}, %Qos2{sess_pid: sess_pid, msg: msg} = state) do
    bcast_msg(msg)
    GenServer.cast(sess_pid, {:msg, PubComp.new(msg.msg_id)})
    {:stop, :normal, state}
  end

  def handle_cast({:next, msg}, state) do
      Logger.error "Unexpected Message: #{inspect msg} for #{inspect __MODULE__}"
      {:stop, :normal, state}
  end

  def handle_info(:timeout, %Qos2{} = state) do
      {:stop, :normal, state}
  end

end
