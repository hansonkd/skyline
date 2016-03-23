defmodule Skyline.Client do
    @moduledoc false
    # Listens for new message on socket.
    # Is repsonsible for listening for new TCP messages, and handling their dispatch

    defstruct socket: nil,
              client_id: nil,
              keep_alive_server_ms: nil,
              auth_info: nil,
              app_config: nil,
              persistent_session: false,
              supervisor_pid: nil

    require Logger
    
    import Supervisor.Spec

    alias Skyline.Socket
    alias Skyline.Client
    alias Skyline.Msg.Decode.Utils, as: Decoder
    alias Skyline.Msg.{Connect, Disconnect}

    def start_link(socket, app_config, _opts \\ []) do
      client = %Client{socket: socket, app_config: app_config}
      IO.inspect client
      pid = spawn_link(fn() -> init(client) end)
      {:ok, pid}
    end
    
    def init(%Client{client_id: client_id, app_config: app_config, socket: socket} = client) do
        IO.puts "Hey"
        #pid = Skyline.ClientSupervisor.start_link(:hey)
        {:ok, pid} = Skyline.ClientSupervisor.start_link(socket, app_config)
        IO.inspect {"WHAT?", pid}
        listen(%{client | supervisor_pid: pid})
    end

    def listen(%Client{socket: socket, client_id: client_id} = client) do
      case Socket.recv(socket, 2) do
        { :ok, data = <<_m :: size(16)>> } ->
            msg = Decoder.decode(data, socket)
            case msg do
              %Connect{} ->
                authenticate(msg, client)
              _other ->
                Logger.warn "#{inspect socket} is not authorized. Closing."
                {:close_connection, "unauthorized"}
            end
        other ->
            Logger.warn "Got unexcected message: #{inspect other}. Closing."
            {:close_connection, "unexcected"}
      end
    end

    def verified_loop(%Client{socket: socket, auth_info: auth_info, client_id: client_id, keep_alive_server_ms: ka_ms} = client) do
      case Socket.recv(socket, 2, ka_ms) do
        { :ok, data = <<_m :: size(16)>> } ->
            msg = Decoder.decode(data, socket)
            case msg do
              nil ->
                Skyline.Events.error(client_id, auth_info, Skyline.MalformedMessage.exception(bytes_recieved: data))
                verified_loop(client)
              _ -> process_msg(msg, client)
            end
        {:ok, nil} ->
            verified_loop(client)
        other ->
            Skyline.Events.error(client_id, auth_info, Skyline.MalformedMessage.exception(bytes_recieved: inspect other))
            {:close_connection, Skyline.MalformedMessage.exception(bytes_recieved: inspect other)}
      end
    end

    def authenticate(%Connect{client_id: client_id} = msg, %Client{socket: socket} = client) do
        case Skyline.Auth.connect(msg, client) do
          {res_msg, %Client{auth_info: auth_info} = new_client} ->
              Skyline.Events.connect(client_id, auth_info)
              Socket.send(socket, res_msg)
              verified_loop(new_client)
          {:error, emsg} ->
              Socket.send(socket, emsg)
              {:close_connection, emsg, client}
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

    defp process_msg(msg, %Client{client_id: client_id, auth_info: auth_info} = client) do
      Skyline.Events.accept_message(client_id, auth_info, msg)
      case msg do
        %Connect{} ->
            Skyline.Events.error(client_id, auth_info, Skyline.AlreadyConnected.exception(client_id: client_id))
            {:stop, :normal, client}
        %Disconnect{} ->
            {:stop, :normal, client}
        _other ->
            case Skyline.Handler.handle_msg(msg, client) do
                {:close_connection, reason} ->
                    Skyline.Events.error(client_id, auth_info, reason)
                    {:close_connection, reason}
                %Client{} = new_client->
                    verified_loop(new_client)
            end
      end
    end
end
