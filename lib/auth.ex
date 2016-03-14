defmodule Skyline.Auth do
    alias Skyline.Msg.{Connect, ConnAck}
    alias Skyline.Session

    # Reconnects an existing server with a new connection
    defp reconnect(server, connection, client_proc) do
      :gen_server.call(server, {:reconnect, connection, client_proc})
    end

    def connect(client, %Connect{} = con) do
      value = case Session.start_link(client, con) do
         {:error, {:already_started, pid}} ->
            {:ok, pid}
         any -> any
       end
      case value do
        {:ok, pid}       -> {:ok, ConnAck.new(:ok), pid}
        {:error, reason} -> {:error, ConnAck.new(reason)}
      end
    end
end
