defmodule Incoming do
  defmacro __using__(_opts) do
    quote do
      def bcast_msg(msg) do
        #GenServer.cast({:via, :gproc, {:p, :l, {:topic, }}}, )
        Skyline.Topic.Dispatcher.broadcast_msg(msg.topic, {:publish, msg})
      end
    end
  end
end

defmodule Skyline.Qos.Incoming.Qos0 do
  @moduledoc """
  Incoming QoS0.

  QoS0 is fire and forget so no observer process is needed.
  """

  use Incoming

  def start(sess_pid, client_id, msg) do
    #pid = spawn_link(fn() ->  bcast_msg(msg) end)
    bcast_msg(msg)
    {:ok, nil}
  end

end
defmodule Skyline.Qos.Incoming.Qos1 do
  @moduledoc """
  Incoming QoS1.

  QoS1 only requires an acknowledgement be sent, so no observer is needed.
  """

  use Incoming
  alias Skyline.Msg.PubAck

  def start(sess_pid, client_id, msg) do
    bcast_msg(msg)
    GenServer.cast(sess_pid, {:msg, PubAck.new(msg.msg_id)})
    {:ok, nil}
  end

end
