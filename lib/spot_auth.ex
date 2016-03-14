defmodule Spotmq.Auth do
    alias Spotmq.Msg.{Connect, ConnAck}
    alias Spotmq.Session

    # Reconnects an existing server with a new connection
    defp reconnect(server, connection, client_proc) do
      :gen_server.call(server, {:reconnect, connection, client_proc})
    end

    def connect(client, %Connect{} = con) do
      ##IO.inspect("TCP.do_connect self=#{inspect self} and con = #{inspect con}")

      value = case Session.start_link({client, con}) do
         {:error, {:already_started, pid}} ->
            ##IO.inspect("Already started") #reconnect(pid, con, client_proc)
            {:ok, pid}
         any -> any
       end
      case value do
        {:ok, pid}       -> {:ok, ConnAck.new(:ok), pid}
        {:error, reason} -> {:error, ConnAck.new(reason)}
      end
    end
end
