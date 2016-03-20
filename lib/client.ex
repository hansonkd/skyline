defmodule Skyline.Client do
    @moduledoc false
    # Listens for new message on socket.
    # Is repsonsible for listening for new TCP messages, and handling their dispatch

    defstruct socket: nil,
              client_id: nil,
              keep_alive_server_ms: nil,
              sess_pid: nil,
              auth_info: nil,
              app_config: nil

    use GenServer

    require Logger

    alias Skyline.Socket
    alias Skyline.Client
    alias Skyline.Msg.Decode.Utils, as: Decoder

    def start_link(client, app, _opts \\ []) do
      state = %Client{socket: client, app_config: app.init}
      GenServer.start_link(__MODULE__, state)
    end

    def init(client) do
      GenServer.cast(self, :listen)
      {:ok, client}
    end

    def handle_cast(:listen, %Client{socket: socket} = st) do
      case Socket.recv(socket, 2) do
        { :ok, data = <<_m :: size(16)>> } ->
            msg = Decoder.decode(data, socket)
            case msg do
              %Skyline.Msg.Connect{} ->
                GenServer.cast(self, {:authenticate, msg})
                {:noreply, st}
              _other ->
                Logger.warn "#{inspect socket} is not authorized. Closing."
                GenServer.cast(self, :listen)
                {:noreply, st}
            end
        other -> Logger.warn "Recieved #{inspect other} instead of bytes. Closing."
                 {:stop, :normal, st}
      end
    end

    def handle_cast(:verified_loop, %Client{socket: socket, keep_alive_server_ms: ka_ms} = state) do
      case Socket.recv(socket, 2, ka_ms) do
        { :ok, data = <<_m :: size(16)>> } ->
            msg = Decoder.decode(data, socket)
            case msg do
              nil ->
                Logger.error "Failed reading data #{inspect data}"
                {:noreply, state}
              _ -> process_msg(msg, state)
            end


        other -> Logger.warn "Recieved #{inspect other} instead of bytes. Closing."
                 {:stop, :normal, state}
      end
    end

    def handle_cast({:authenticate, msg}, %Client{socket: socket} = state) do
        case Skyline.Auth.connect(socket, msg, state) do
          {:ok, res_msg, sess_pid, auth_info} ->
              Socket.send(socket, res_msg)
              Logger.info "Authenticated #{inspect msg.client_id} with auth_info #{inspect auth_info}"
              new_state = %{
                state | client_id: msg.client_id,
                        keep_alive_server_ms: msg.keep_alive_server_ms,
                        sess_pid: sess_pid,
                        auth_info: auth_info}
              GenServer.cast(self, :verified_loop)
              {:noreply, new_state}
          {:error, emsg} ->
              Socket.send(socket, emsg)
              {:stop, :normal, state}
        end
    end

    def terminate(reason, %Client{socket: socket, sess_pid: sess_pid}) do
      Logger.debug "#{inspect socket} terminated becuase of #{inspect reason}"
      Socket.close(socket)
      if sess_pid && Process.alive?(sess_pid) do
        GenServer.stop(sess_pid, :normal, :infinity)
      end
      :ok
    end

    defp process_msg(msg, %Client{client_id: client_id} = state) do
      case msg do
        %Skyline.Msg.Connect{} ->
            Logger.warn "Already authorized. Closing."
            {:stop, :normal, state}
        %Skyline.Msg.Disconnect{} ->
            Logger.info "Disconnecting on request of client #{inspect client_id}"
            {:stop, :normal, state}
        _other ->
            Logger.debug "Recieved message. #{inspect msg}."
            case Skyline.Handler.handle_msg(msg, state) do
              {:close_connection, reason} ->
                Logger.warn "Processing asked to close connection #{inspect client_id} because of #{inspect reason}."
                {:stop, :normal, state}
              :ok ->
                GenServer.cast(self, :verified_loop)
                {:noreply, state}
            end
      end
    end
end
