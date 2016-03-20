defmodule Skyline.Auth do
  alias Skyline.Msg.{Connect, ConnAck}
  alias Skyline.Session

  @type connect_response :: {:ok, ConnAck.t, pid, any} | {:error, ConnAck.t}
  # Reconnects an existing server with a new connection
  defp reconnect(server, connection, client_proc) do
    :gen_server.call(server, {:reconnect, connection, client_proc})
  end

  @spec connect(Skyline.socket, Skyline.Msg.Connect.t, Skyline.Client.t) :: connect_response
  def connect(client, %Connect{} = con_msg, %Skyline.Client{app: app}) do
    case Skyline.Auth.Protocol.new_connection(app.auth, con_msg) do
      {:ok, auth_state} ->
          case Session.start_link(client, con_msg, auth_state) do
             {:error, {:already_started, pid}} ->
                {:ok, ConnAck.new(:ok), pid, auth_state}
             {:ok, pid} ->
                {:ok, ConnAck.new(:ok), pid, auth_state}
           end
      {:error, reason} -> {:error, ConnAck.new(reason)}
    end
  end

end
