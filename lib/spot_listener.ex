defmodule Spotmq.Listener do
    use GenServer

    import Socket
    alias Spotmq.Session
    alias Spotmq.Msg.Decode.Utils, as: Decoder
    alias Spotmq.Handler


    defmodule State do
      defstruct socket: nil,
                transport: nil,
                conn_msg: nil,
                sess_pid: nil
    end

    #def start_link(client, _opts \\ []) do
    #  state = %State{socket: client}
    #  GenServer.start_link(__MODULE__, state)
    #end

    def start_link(ref, socket, transport, opts) do
      #pid = spawn_link(__MODULE__, :init, [ref, socket, transport, opts])
      IO.puts("Start link!")
      GenServer.start_link(__MODULE__, {ref, socket, transport, opts})
    end

    def init({ref, socket, transport, _Opts = []}) do
      IO.puts("Init!")
      #:ok = :ranch.accept_ack(ref)
      IO.puts("Accepted!")
      GenServer.cast(self, :listen)
      {:ok, %State{socket: socket, transport: transport}}
    end

    defp recv(%State{socket: socket, transport: transport}, num, timeout \\ :infinity) do
      transport.recv(socket, num, timeout)
    end

    def handle_cast(:listen, %State{socket: socket, transport: transport} = st) do
      case recv(st, 2) do
        { :ok, data = <<_m :: size(16)>> } ->
            msg = Decoder.decode(data, socket, transport)
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

    def handle_cast(:verified_loop, %State{socket: socket, transport: transport, sess_pid: sess_pid, conn_msg: conn_msg} = state) do
      #IO.inspect({"timeout", keep_alive})
      case recv(state, 2, conn_msg.keep_alive_server_ms) do
        { :ok, data = <<_m :: size(16)>> } ->
            msg = Decoder.decode(data, socket, transport)
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
    def terminate(reason, %State{socket: socket, transport: transport, sess_pid: sess_pid}) do
      transport.close(socket)
      #GenServer.stop(sess_pid, :normal, :infinity)
      :ok
    end
    def handle_cast({:authenticate, msg}, %State{socket: socket, transport: transport} = state) do
        case Spotmq.Auth.connect(socket, transport, msg) do
          {:ok, smsg, sess_pid} ->
              Session.send_to_socket(socket, transport, smsg)
              new_state = %{state | conn_msg: msg, sess_pid: sess_pid}
              GenServer.cast(self, :verified_loop)
              {:noreply, new_state}
          {:error, emsg} ->
              Session.send_to_socket(socket, transport, emsg)
              {:stop, :normal, state}
        end
    end
end
