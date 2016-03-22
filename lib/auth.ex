defmodule Skyline.Auth do
  @moduledoc false

  alias Skyline.Msg.{Connect, ConnAck}
  alias Skyline.Session
  alias Skyline.AppConfig

  @type connect_response :: {:ok, ConnAck.t, pid, any} | {:error, ConnAck.t}
  # Reconnects an existing server with a new connection
  defp reconnect(server, connection, client_proc) do
    :gen_server.call(server, {:reconnect, connection, client_proc})
  end

  @spec connect(Skyline.socket, Skyline.Msg.Connect.t, Skyline.Client.t) :: connect_response
  def connect(client, %Connect{client_id: client_id} = con_msg,
              %Skyline.Client{app_config: %AppConfig{auth_module: auth_mod, auth_opts: auth_opts}}) do
    case auth_mod.new_connection(con_msg, auth_opts) do
      {:ok, auth_info} ->
          case Session.start_link(client, client_id, auth_info) do
             {:error, {:already_started, pid}} ->
                {:ok, ConnAck.new(:ok), pid, auth_info}
             {:ok, pid} ->
                {:ok, ConnAck.new(:ok), pid, auth_info}
           end
      {:error, reason} -> {:error, ConnAck.new(reason)}
    end
  end
  
end
