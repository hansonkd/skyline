defmodule Skyline.Auth do
  @moduledoc false

  import Skyline.Handler

  alias Skyline.Msg.{Connect, ConnAck}
  alias Skyline.Session
  alias Skyline.AppConfig
  alias Skyline.Client

  @type connect_response :: {:ok, ConnAck.t, pid, any} | {:error, ConnAck.t}
  # Reconnects an existing server with a new connection
  defp reconnect(server, connection, client_proc) do
    :gen_server.call(server, {:reconnect, connection, client_proc})
  end


  @spec connect(Skyline.Msg.Connect.t, Skyline.Client.t) :: connect_response
  def connect(%Connect{client_id: client_id,
                       clean_session: clean_session} = con_msg,
              %Skyline.Client{app_config: %AppConfig{auth_module: auth_mod, auth_opts: auth_opts}} = client) do

    case auth_mod.new_connection(con_msg, auth_opts) do
      {:ok, auth_info} ->
          
          :ets.insert(:session_msg_ids, {con_msg.client_id, 0})

          new_client = %{
            client | client_id: con_msg.client_id,
                     keep_alive_server_ms: con_msg.keep_alive_server_ms,
                     auth_info: auth_info,
                     persistent_session: not clean_session}

          
          {loaded, final_client} = Session.start_session(new_client)

          {ConnAck.new(:ok, loaded), final_client}

      {:error, reason} -> {:error, ConnAck.new(reason)}
    end
  end

end
