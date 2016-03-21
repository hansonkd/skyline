defmodule Skyline.Auth do
  @moduledoc false

  alias Skyline.Msg.{Connect, ConnAck}
  alias Skyline.Session

  @type connect_response :: {:ok, ConnAck.t, pid, any} | {:error, ConnAck.t}
  # Reconnects an existing server with a new connection
  defp reconnect(server, connection, client_proc) do
    :gen_server.call(server, {:reconnect, connection, client_proc})
  end

  @spec connect(Skyline.socket, Skyline.Msg.Connect.t, Skyline.Client.t) :: connect_response
  def connect(client, %Connect{client_id: client_id} = con_msg, %Skyline.Client{app_config: config}) do
    case Skyline.Auth.Protocol.new_connection(config.auth, con_msg) do
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
