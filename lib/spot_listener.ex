defmodule Spotmq.Listener do
    use GenServer
    import Socket
    alias Spotmq.Session
    alias Spotmq.Msg.Decode.Utils, as: Decoder
    alias Spotmq.Handler


    defmodule State do
      defstruct socket: nil,
                conn_msg: nil,
                sess_pid: nil
    end

    def start_link(client, _opts \\ []) do
      state = %State{socket: client}
      GenServer.start_link(__MODULE__, state)
    end

    def init(client) do
      GenServer.cast(self, :listen)
      {:ok, client}
    end

    def handle_cast(:listen, %State{socket: socket} = st) do
      case Socket.Stream.recv(socket, 2) do
        { :ok, data = <<_m :: size(16)>> } ->
            msg = Decoder.decode(data, read_byte(socket), fn n -> read_bytes(socket, n) end)
            case msg do
              %Spotmq.Msg.Connect{} ->
                GenServer.cast(self, {:authenticate, msg})
                {:noreply, st}
              _other -> #IO.inspect("Have not recieved auth")
                GenServer.cast(self, :listen)
                {:noreply, st}
            end
        #{:error, :closed } -> :ok
        other -> #IO.inspect other
                 {:stop, :normal, st}
      end
    end

    def handle_cast(:verified_loop, %State{socket: socket, sess_pid: sess_pid, conn_msg: conn_msg} = state) do
      #IO.inspect({"timeout", keep_alive})
      case Socket.Stream.recv(socket, 2, [timeout: conn_msg.keep_alive_server_ms]) do
        { :ok, data = <<_m :: size(16)>> } ->
            msg = Decoder.decode(data, read_byte(socket), fn n -> read_bytes(socket, n) end)
            case msg do
              %Spotmq.Msg.Connect{} ->
                  #IO.puts("Only one connect message per connection...")
                  {:stop, :normal, state}
              %Spotmq.Msg.Disconnect{} ->
                  #IO.puts("Disconnecting on request of client")
                  {:stop, :normal, state}
              _other ->
                  Spotmq.Handler.handle_msg(msg, sess_pid, conn_msg)
                  GenServer.cast(self, :verified_loop)
                  {:noreply, state}
            end
        #{:ok, nil} ->
        #  #IO.puts "Got nil"
        #  authenticated_loop(client, sess_pid, keep_alive)
        # {:error, :closed } -> GenServer.stop(sess_pid, :socket_closed, :infinity)
        other -> #IO.inspect {"WEROD SHIT!", other}
                 {:stop, :normal, state}
                 #authenticated_loop(client, sess_pid, keep_alive)
      end
    end
    def terminate(reason, %State{socket: socket, sess_pid: sess_pid}) do
      Socket.close(socket)
      GenServer.stop(sess_pid, :normal, :infinity)
      :ok
    end
    def handle_cast({:authenticate, msg}, %State{socket: socket} = state) do

        case Spotmq.Auth.connect(socket, msg) do
          {:ok, smsg, sess_pid} ->
              Session.send_to_socket(socket, smsg)
              new_state = %{state | conn_msg: msg, sess_pid: sess_pid}
              GenServer.cast(self, :verified_loop)
              {:noreply, new_state}
          {:error, emsg} ->
              Session.send_to_socket(socket, emsg)
              {:stop, :normal, state}
        end
    end

  defp read_byte(socket) do
    fn ->
      case Socket.Stream.recv(socket, 1) do
        {:ok, byte} -> {byte, read_byte(socket)}
        {:error, reason} -> #IO.inspect("read_byte: receiving 1 byte failed with #{inspect reason}")
      end
    end
  end

  defp read_bytes(socket, 0), do: ""
  defp read_bytes(socket, nr) do
      result = case Socket.Stream.recv(socket, nr) do
        {:ok, bytes} -> bytes
        # {:error, reason} -> Lager.error("read_bytes: receiving #{nr} bytes failed with #{inspect reason}")
        any ->
          #IO.inspect("Received a strange message: #{inspect any}")
          any
      end
      result
    end

end
