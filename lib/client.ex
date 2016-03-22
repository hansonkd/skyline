defmodule Skyline.Client do
    @moduledoc false
    # Listens for new message on socket.
    # Is repsonsible for listening for new TCP messages, and handling their dispatch

    defstruct socket: nil,
              client_id: nil,
              keep_alive_server_ms: nil,
              auth_info: nil,
              app_config: nil,
              persistent_session: false

    use GenServer

    require Logger

    alias Skyline.Socket
    alias Skyline.Client
    alias Skyline.Msg.Decode.Utils, as: Decoder
    alias Skyline.Msg.{Connect, Disconnect}

    def start_link(client, app_config, _opts \\ []) do
      state = %Client{socket: client, app_config: app_config}
      GenServer.start_link(__MODULE__, state)
    end

    def init(client) do
      GenServer.cast(self, :listen)
      {:ok, client}
    end

    def handle_cast(:listen, %Client{socket: socket, client_id: client_id} = state) do
      case Socket.recv(socket, 2) do
        { :ok, data = <<_m :: size(16)>> } ->
            msg = Decoder.decode(data, socket)
            case msg do
              %Connect{} ->
                GenServer.cast(self, {:authenticate, msg})
                {:noreply, state}
              _other ->
                Logger.warn "#{inspect socket} is not authorized. Closing."
                {:stop, :normal, state}
            end
        other ->
            Logger.warn "Got unexcected message: #{inspect other}. Closing."
            {:stop, :normal, state}
      end
    end

    def handle_cast(:verified_loop, %Client{socket: socket, auth_info: auth_info, client_id: client_id, keep_alive_server_ms: ka_ms} = state) do
      case Socket.recv(socket, 2, ka_ms) do
        { :ok, data = <<_m :: size(16)>> } ->
            msg = Decoder.decode(data, socket)
            case msg do
              nil ->
                Skyline.Events.error(client_id, auth_info, Skyline.MalformedMessage.exception(bytes_recieved: data))
                {:noreply, state}
              _ -> process_msg(msg, state)
            end
        {:ok, nil} ->
            {:noreply, state}
        other ->
            Skyline.Events.error(client_id, auth_info, Skyline.MalformedMessage.exception(bytes_recieved: inspect other))
            {:stop, :normal, state}
      end
    end

    def handle_cast({:authenticate, %Connect{client_id: client_id} = msg}, %Client{socket: socket} = state) do
        case Skyline.Auth.connect(msg, state) do
          {res_msg, %Client{auth_info: auth_info} = client} ->
              Skyline.Events.connect(client_id, auth_info)
              Socket.send(socket, res_msg)
              GenServer.cast(self, :verified_loop)
              {:noreply, client}
          {:error, emsg} ->
              Socket.send(socket, emsg)
              {:stop, :normal, state}
        end
    end

    def handle_cast({:force_close_connection, reason}, %Client{client_id: client_id, auth_info: auth_info} = state) do
      Skyline.Events.error(client_id, auth_info, reason)
      {:stop, :normal, state}
    end

    def terminate(reason, %Client{socket: socket, auth_info: auth_info, client_id: client_id}) do
      Logger.debug "#{inspect socket} terminated becuase of #{inspect reason}"
      Socket.close(socket)
      Skyline.Events.disconnect(client_id, auth_info)
      :ok
    end

    defp process_msg(msg, %Client{client_id: client_id, auth_info: auth_info} = state) do
      Skyline.Events.accept_message(client_id, auth_info, msg)
      case msg do
        %Connect{} ->
            Skyline.Events.error(client_id, auth_info, Skyline.AlreadyConnected.exception(client_id: client_id))
            {:stop, :normal, state}
        %Disconnect{} ->
            {:stop, :normal, state}
        _other ->
            case Skyline.Handler.handle_msg(msg, state) do
                {:close_connection, reason} ->
                    Skyline.Events.error(client_id, auth_info, reason)
                    {:stop, :normal, state}
                %Client{} = new_state->
                    GenServer.cast(self, :verified_loop)
                    {:noreply, new_state}
            end
      end
    end
end
