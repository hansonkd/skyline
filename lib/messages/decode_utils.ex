defmodule Skiline.Msg.Decode.Utils do
    use Bitwise
    alias Skiline.Msg.FixedHeader
    alias Skiline.Msg

    @spec decode(binary, Skiline.socket) :: Skiline.message_type
    def decode(msg = <<_m :: size(16)>>, socket) do
      header = decode_fixheader(msg, socket)
      var_m = read_bytes(socket, header.length)
      decode_message(var_m, header)
    end

    defp decode_fixheader(<<type :: size(4), dup :: size(1), qos :: size(2),
                 retain :: size(1), len :: size(8)>>, socket) do
      FixedHeader.new(
        binary_to_msg_type(type),
        (dup == 1),
        binary_to_qos(qos),
        (retain == 1),
        binary_to_length(<<len>>, socket)
      )
    end

    def decode_message(msg, h = %FixedHeader{message_type: msg_type}) do
      mod = case msg_type do
        :publish -> Msg.PublishReq
        :ping_req -> Msg.PingReq
        :ping_resp -> Msg.PingResp
        :disconnect -> Msg.Disconnect
        :pub_ack -> Msg.PubAck
        :pub_rec -> Msg.PubRec
        :subscribe -> Msg.Subscribe
        :unsubscribe -> Msg.Unsubscribe
        :sub_ack -> Msg.SubAck
        :connect -> Msg.Connect
        :conn_ack -> Msg.ConnAck
      end
      mod.decode_body(msg, h)
    end

    @spec get_msgid(binary) :: pos_integer
    def get_msgid(<<id :: unsigned-integer-size(16)>>) do
      id
    end
    def get_msgid(<<id :: unsigned-integer-size(16)>>) do
      id
    end

    @spec utf8_list(binary, [String.t]) :: [String.t]
    def utf8_list(<<>>, acc) do
      Enum.reverse acc
    end
    def utf8_list(content, acc) do
      {t, rest} = utf8(content)
      utf8_list(rest, [t | acc])
    end

    @spec utf8(binary) :: String.t
    def utf8(<<length :: integer-unsigned-size(16), content :: bytes-size(length), rest :: binary>>) do
      {content, rest}
    end
    def utf8(nil) do
      {"", <<>>}
    end

    @spec binary_to_length(binary, pos_integer, Skiline.socket) :: pos_integer
    defp binary_to_length(_bin, count = 0, _readByte) do
      raise "Invalid length"
    end
    defp binary_to_length(<<overflow :: size(1), len :: size(7)>>, count \\ 4, socket) do
      case overflow do
        1 ->
          byte = read_bytes(socket, 1)
          len + (binary_to_length(byte, count - 1, socket) <<< 7)
        0 -> len
      end
    end

    @spec binary_to_qos(binary) :: Skiline.qos_type
    def binary_to_qos(bin) do
      case bin do
        0 -> :fire_and_forget
        1 -> :at_least_once
        2 -> :exactly_once
        3 -> :reserved
      end
    end

    @spec binary_to_msg_type(binary) :: Skiline.msg_type
    defp binary_to_msg_type(bin) do
      case bin do
        0 -> :reserved
        1 -> :connect
        2 -> :conn_ack
        3 -> :publish
        4 -> :pub_ack
        5 -> :pub_rec
        6 -> :pub_rel
        7 -> :pub_comp
        8 -> :subscribe
        9 -> :sub_ack
        10 -> :unsubscribe
        11 -> :sub_ack
        12 -> :ping_req
        13 -> :ping_resp
        14 -> :disconnect
        15 -> :reserved
      end
    end

    defp read_bytes(socket, 0), do: ""
    defp read_bytes(socket, nr) do
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
