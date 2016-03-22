defmodule Skyline.Socket do
    @moduledoc false

    alias Skyline.Msg.Encode

    def listen(port) do
      Socket.TCP.listen!(port, [packet: 0,
				                        reuseaddr: true,
                                binary: true,
                                nodelay: false,
                                active: true,
                                backlog: 30,
				                        mode: :active])
    end

    @doc "An exposed helper pipe for sending a message to a socket"
    @spec send(Skyline.socket, Skyline.skyline_msg) :: :ok
    def send(socket, msg) do
      send_binary_to_socket(socket, Encode.encode(msg))
    end

    defp send_binary_to_socket(socket, raw_msg) do
      Socket.Stream.send(socket, raw_msg)
    end

    def close(socket) do
      Socket.close(socket)
    end

    def recv(socket, nr, timeout \\ :infinity) do
      Socket.Stream.recv(socket, nr, timeout: timeout)
    end
    def read_bytes(socket, 0) do
      <<>>
    end
    def read_bytes(socket, nr, timeout \\ :infinity) do
      result = case recv(socket, nr, timeout) do
        {:ok, bytes} -> bytes
        any ->
          any
      end
      result
    end
end
