defmodule Skyline.Client do
    @moduledoc """
    Listens for new message on socket.

    Is repsonsible for listening for new TCP messages, and handling their dispatch
    """
    defstruct socket: nil,
              client_id: nil,
              keep_alive_server_ms: nil,
              sess_pid: nil,
              auth_state: nil,
              app: nil

    use GenServer

    import Socket
    alias Skyline.Client
    alias Skyline.Session
    alias Skyline.Msg.Decode.Utils, as: Decoder
    alias Skyline.Handler

    def start_link(client, app, _opts \\ []) do
      state = %Client{socket: client, app: app}
      GenServer.start_link(__MODULE__, state)
    end

    def init(client) do
      GenServer.cast(self, :listen)
      {:ok, client}
    end

    def handle_cast(:listen, %Client{socket: socket} = st) do
      case Socket.Stream.recv(socket, 2) do
        { :ok, data = <<_m :: size(16)>> } ->
            msg = Decoder.decode(data, socket)
            case msg do
              %Skyline.Msg.Connect{} ->
                GenServer.cast(self, {:authenticate, msg})
                {:noreply, st}
              _other -> #IO.inspect("Have not recieved auth")
                GenServer.cast(self, :listen)
                {:noreply, st}
            end
        #{:error, :closed } -> :ok
        other -> IO.inspect other
                 {:stop, :normal, st}
      end
    end

    def handle_cast(:verified_loop, %Client{socket: socket, sess_pid: sess_pid, keep_alive_server_ms: ka_ms} = state) do
      #IO.inspect({"timeout", keep_alive})
      case Socket.Stream.recv(socket, 2, [timeout: ka_ms]) do
        { :ok, data = <<_m :: size(16)>> } ->
            msg = Decoder.decode(data, socket)
            GenServer.cast(self, {:handle_msg, msg})
            {:noreply, state}
        other -> IO.inspect {"WEROD SHIT!", other}
                 {:stop, :normal, state}
                 #authenticated_loop(client, sess_pid, keep_alive)
      end
    end

    def handle_cast({:handle_msg, msg}, state) do
      case msg do
        %Skyline.Msg.Connect{} ->
            #IO.puts("Only one connect message per connection...")
            {:stop, :normal, state}
        %Skyline.Msg.Disconnect{} ->
            #IO.puts("Disconnecting on request of client")
            {:stop, :normal, state}
        _other ->
            IO.puts("Message #{inspect msg}")
            new_state = Skyline.Handler.handle_msg(msg, state)
            GenServer.cast(self, :verified_loop)
            {:noreply, new_state}
      end
    end

    #def terminate(reason, %Client{socket: socket, sess_pid: sess_pid}) do
    #  IO.puts("Reason #{reason}")
    #  Socket.close(socket)
    #  GenServer.stop(sess_pid, :normal, :infinity)
    #  :ok
    #end

    def handle_cast({:authenticate, msg}, %Client{socket: socket} = state) do
        case Skyline.Auth.Dispatcher.connect(socket, msg, state) do
          {:ok, smsg, sess_pid, auth_state} ->
              Skyline.Socket.send(socket, smsg)
              new_state = %{
                state | client_id: msg.client_id,
                        keep_alive_server_ms: msg.keep_alive_server_ms,
                        sess_pid: sess_pid,
                        auth_state: auth_state}
              GenServer.cast(self, :verified_loop)
              {:noreply, new_state}
          {:error, emsg} ->
              Skyline.Socket.send(socket, emsg)
              {:stop, :normal, state}
        end
    end
end
