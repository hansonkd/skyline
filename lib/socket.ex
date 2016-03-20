defmodule Skyline.Socket do
    @moduledoc """
    Socket aliases.
    """

    alias Skyline.Msg.Encode

    def listen(port) do
      Socket.TCP.listen!(port, [packet: 0,
				                        reuseaddr: true,
                                binary: true,
                                nodelay: true,
                                active: true,
                                backlog: 30,
				                        mode: :active])
    end

    @doc "An exposed helper method for sending a message to a socket"
    @spec send(Skyline.socket, Skyline.skyline_msg) :: :ok
    def send(socket, msg) do
      send_binary_to_socket(socket, Encode.encode(msg))
    end

    defp send_binary_to_socket(socket, raw_msg) do
      Socket.Stream.send(socket, raw_msg)
    end

    def read_bytes(socket, nr) do
        result = case Socket.Stream.recv(socket, nr) do
          {:ok, bytes} -> bytes
          # {:error, reason} -> Lager.error("read_bytes: receiving #{nr} bytes failed with #{inspect reason}")
          any ->
            #IO.inspect("Received a strange message: #{inspect any}")
            any
        end
        result
      end
end
