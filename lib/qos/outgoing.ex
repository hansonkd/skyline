defmodule Outgoing do
  @moduledoc false

  defmacro __using__(_opts) do

    quote do
      alias Skyline.Socket

      def max_tries do
        Application.get_env(:skyline, :qos_timeout, 5)
      end
      def timeout do
        Application.get_env(:skyline, :qos_timeout, 15000)
      end

      defp finish(sub_pid) do
         send(sub_pid, {:finished, self})
      end

    end
  end
end

defmodule Skyline.Qos.Outgoing.Qos0 do
  @moduledoc false

  # Outgoing QoS0.
  #
  # QoS0 is fire and forget so no observer process is needed.
  defstruct [:sub_pid, :client_id, :socket]

  use Outgoing

  def start(socket, sub_pid, client_id, msg) do

    #GenServer.cast(sub_pid, {:finish_msg, msg.msg_id})
    state = %__MODULE__{socket: socket, client_id: client_id, sub_pid: sub_pid}
    pid = spawn_link(fn() -> init(msg, state) end)
    {:ok, pid}
  end

  def init(msg, state) do
    send_to_socket(msg, state)
    loop(state)
  end

  def loop(state) do
    receive do
      {:send_msg, msg} ->
        send_to_socket(msg, state)
    end
    loop(state)
  end

  defp send_to_socket(msg, %__MODULE__{sub_pid: sub_pid, socket: socket}) do
        Socket.send(socket, msg)
        finish(sub_pid)
  end
end

defmodule Skyline.Qos.Outgoing.Qos1 do
  @moduledoc false

  # Outgoing QoS1.
  #
  # QoS1 requires an acknowledgement, so we much have a process to wait for the client's
  # acknowledgement or resend.

  defstruct last_msg: nil,
            socket: nil,
            sub_pid: nil,
            tries: 0

  use GenServer
  use Outgoing

  require Logger

  alias Skyline.Qos.Outgoing.Qos1
  alias Skyline.Msg.PubAck


  def start(socket, sub_pid, client_id, msg) do
    state = %Qos1{socket: socket, sub_pid: sub_pid, last_msg: msg}
    GenServer.start_link(__MODULE__, {msg, state}, name: {:global, {:qos_send, client_id, msg.msg_id}})
  end

  def init({msg, %Qos1{socket: socket} = state}) do
    Socket.send(socket, msg)
    {:ok, %{state | last_msg: %{msg | duplicate: true}}, timeout}
  end

  defp loop(%Qos1{sub_pid: sub_pid, socket: socket} = state) do
     {:next, %PubAck{msg_id: msg_id} ->
        finish(sub_pid)
  end

  def handle_cast(}, %Qos1{sub_pid: sub_pid, msg: msg} = state) do
    if msg_id == msg.msg_id do
      GenServer.cast(sub_pid, {:finish_msg, msg.msg_id})
    else
      Logger.error "Recieved PubAck for message \##{inspect msg_id} when \##{inspect msg.msg_id} is being processed"
    end
    {:stop, :normal, state}
  end

  def handle_cast({:next, msg}, state) do
      Logger.error "Unexpected Message: #{inspect msg} for #{inspect __MODULE__}"
      {:stop, :normal, state}
  end

  def handle_info(:timeout, %Qos1{socket: socket, msg: msg, sub_pid: sub_pid, tries: tries} = state) do
    if tries < max_tries do
        Socket.send(socket, msg)
        {:noreply, %{state | tries: tries + 1}, timeout}
    else
        GenServer.cast(sub_pid, {:finish_msg, msg.msg_id})
        {:stop, :normal, state}
    end
  end
end

defmodule Skyline.Qos.Outgoing.Qos2 do
  @moduledoc false

  # Outgoing QoS2.
  #
  # QoS2 requires an acknowledgement messages, so we much have a process to wait for the client's
  # acknowledgement or resend.

  defstruct msg: nil,
            socket: nil,
            sub_pid: nil,
            tries: 0,
            expected: nil

  require Logger

  use GenServer
  use Outgoing

  alias Skyline.Qos.Outgoing.Qos2
  alias Skyline.Msg.{PubAck, PubRec, PubComp, PubRel}


  def start(socket, sub_pid, client_id, msg) do
    state = %Qos2{socket: socket, sub_pid: sub_pid, msg: msg}
    GenServer.start_link(__MODULE__, {msg, state}, name: {:global, {:qos_send, client_id, msg.msg_id}})
  end

  def init({msg, %Qos2{socket: socket} = state}) do
    Socket.send(socket, msg)
    {:ok, %{state | msg: %{msg | duplicate: true}, expected: :pubrec}, timeout}
  end

  def handle_cast({:next, %PubRec{msg_id: msg_id}}, %Qos2{socket: socket, msg: msg, expected: :pubrec} = state) do
      if msg_id == msg.msg_id do
        new_msg = PubRel.new(msg.msg_id)
        Send.socket(socket, new_msg)
        {:noreply, %{state | expected: :pubcomp}}
      else
        Logger.error "Recieved PubRec for message \##{inspect msg_id} when \##{inspect msg.msg_id} is being processed"
        {:stop, :normal, state}
      end
  end

  def handle_cast({:next, %PubComp{msg_id: msg_id}}, %Qos2{sub_pid: sub_pid, msg: msg, expected: :pubcomp} = state) do
      if msg_id == msg.msg_id do
        GenServer.cast(sub_pid, {:finish_msg, msg_id})
        {:stop, :normal, state}
      else
        Logger.error "Recieved PubComp for message \##{inspect msg_id} when \##{inspect msg.msg_id} is being processed"
        {:stop, :normal, state}
      end
  end

  def handle_cast({:next, msg}, state) do
        Logger.error "Unexpected Message: #{inspect msg} for #{inspect __MODULE__}"
        {:stop, :normal, state}
  end

  def handle_info(:timeout, %Qos2{socket: socket, sub_pid: sub_pid, msg: msg, tries: tries} = state) do
    Logger.error "Timed out"
    if tries == :infinity or tries < max_tries do
        Socket.send(socket, msg)
        {:noreply, %{state | tries: tries + 1}, timeout}
    else
        GenServer.cast(sub_pid, {:finish_msg, msg.msg_id})
        {:stop, :normal, state}
    end
  end
end
