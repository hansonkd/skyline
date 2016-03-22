defmodule Skyline.Auth do
  @moduledoc false

  import Skyline.Handler

  alias Skyline.Msg.{Connect, ConnAck}
  alias Skyline.Session
  alias Skyline.AppConfig
  alias Skyline.Client

  use Skyline.Amnesia.Session.SessionDatabase

  @type connect_response :: {:ok, ConnAck.t, pid, any} | {:error, ConnAck.t}
  # Reconnects an existing server with a new connection
  defp reconnect(server, connection, client_proc) do
    :gen_server.call(server, {:reconnect, connection, client_proc})
  end


  @spec connect(Skyline.socket, Skyline.Msg.Connect.t, Skyline.Client.t) :: connect_response
  def connect(client_socket, %Connect{client_id: client_id,
                                      clean_session: clean_session} = con_msg,
              %Skyline.Client{app_config: %AppConfig{auth_module: auth_mod, auth_opts: auth_opts}} = client) do

    case auth_mod.new_connection(con_msg, auth_opts) do
      {:ok, auth_info} ->
          {:ok, pid} = handle_new_connection(client_socket, client_id, auth_info)

          new_client = %{
            client | client_id: con_msg.client_id,
                     keep_alive_server_ms: con_msg.keep_alive_server_ms,
                     sess_pid: pid,
                     auth_info: auth_info,
                     persistent_session: not clean_session}

          {loaded, final_client} = Amnesia.transaction do
              fresh_session = %StoredSession{client_id: client_id, topics: []}
              case StoredSession.read(client_id) do
                nil ->
                    if not clean_session do
                      fresh_session |> StoredSession.write
                    end
                    {false, new_client}
                %StoredSession{topics: topics} = session ->
                  IO.puts "Loaded session #{inspect session}"
                  if clean_session do
                      StoredSession.delete(session)
                      {false, new_client}
                  else
                      sub_msg = Skyline.Msg.Subscribe.new(topics, 0)
                      case Skyline.Handler.handle_subscribe(topics, sub_msg, [], new_client) do
                        {:close_connection, reason} ->
                            StoredSession.delete(session)
                            fresh_session |> StoredSession.write
                            {false, new_client}
                        {_qos, %Client{} = after_sub} ->
                            {true, after_sub}
                      end
                  end
              end
          end

          {ConnAck.new(:ok, loaded), final_client}

      {:error, reason} -> {:error, ConnAck.new(reason)}
    end
  end

  defp handle_new_connection(client_socket, client_id, auth_info) do
    case Session.start_link(client_socket, client_id, auth_info) do
       {:error, {:already_started, pid}} ->
          GenServer.stop(pid)
          handle_new_connection(client_socket, client_id, auth_info)
       {:ok, pid} ->
          {:ok, pid}
     end
  end
end
