defmodule Skiline.Listener do
    @moduledoc """
    Listener

    Is repsonsible for listening for new TCP messages, and handling their dispatch
    """
    use GenServer

    import Socket
    alias Skiline.Session
    alias Skiline.Msg.Decode.Utils, as: Decoder
    alias Skiline.Handler


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
            msg = Decoder.decode(data, socket)
            case msg do
              %Skiline.Msg.Connect{} ->
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
            msg = Decoder.decode(data, socket)
            case msg do
              %Skiline.Msg.Connect{} ->
                  #IO.puts("Only one connect message per connection...")
                  {:stop, :normal, state}
              %Skiline.Msg.Disconnect{} ->
                  #IO.puts("Disconnecting on request of client")
                  {:stop, :normal, state}
              _other ->
                  Skiline.Handler.handle_msg(msg, sess_pid, conn_msg)
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
        case Skiline.Auth.connect(socket, msg) do
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
end
