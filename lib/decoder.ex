defmodule Spotmq.Decoder do

  use Bitwise
  alias Spotmq.Msg

  def decode(msg = <<_m :: size(16)>>, readByte, readMsg) do
    header = decode_fixheader(msg, readByte)
    IO.puts("\n\n\nHeader = #{inspect header}\n\n\n")
    var_m = readMsg.(header.length)
    #Lager.info("decoding remaing messages #{inspect var_m}")
    IO.puts("\n\nvar_m = #{inspect var_m}\n\n\n")
    decode_message(var_m, header)
  end

  def decode_fixheader(<<type :: size(4), dup :: size(1), qos :: size(2),
               retain :: size(1), len :: size(8)>>, readByte) do
    Spotmq.Msg.fixed_header(binary_to_msg_type(type),
      (dup == 1), binary_to_qos(qos),(retain == 1),
      binary_to_length(<<len>>, readByte))
  end

  def decode_message(msg, h = %Msg.FixedHeader{message_type: :publish}) do
    decode_publish(msg, h)
  end
  def decode_message(<<>>, %Msg.FixedHeader{message_type: :ping_req, length: 0}) do
     Spotmq.Msg.ping_req()
  end
  def decode_message(<<>>, %Msg.FixedHeader{message_type: :ping_resp, length: 0}) do
    Spotmq.Msg.ping_resp()
  end
  def decode_message(<<>>, %Msg.FixedHeader{message_type: :disconnect, length: 0}) do
    Spotmq.Msg.disconnect()
  end
  def decode_message(msg, h = %Msg.FixedHeader{message_type: :pub_ack}) do
    Spotmq.Msg.pub_ack(get_msgid(msg))
  end
  def decode_message(msg, h = %Msg.FixedHeader{message_type: :pub_rec}) do
    Spotmq.Msg.pub_rec(get_msgid(msg))
  end
  def decode_message(msg, h = %Msg.FixedHeader{message_type: :pub_rel, qos: :at_least_once, duplicate: dup}) do
    Spotmq.Msg.pub_rel(get_msgid(msg), dup)
  end
  def decode_message(msg, h = %Msg.FixedHeader{message_type: :pub_comp}) do
    Spotmq.Msg.pub_comp(get_msgid(msg))
  end
  def decode_message(msg, h = %Msg.FixedHeader{message_type: :unsub_ack}) do
    Spotmq.Msg.unsub_ack(get_msgid(msg))
  end
  def decode_message(msg, h = %Msg.FixedHeader{message_type: :subscribe}) do
    decode_subscribe(msg, h)
  end
  def decode_message(msg, h = %Msg.FixedHeader{message_type: :unsubscribe}) do
    decode_unsubscribe(msg)
  end
  def decode_message(msg, h = %Msg.FixedHeader{message_type: :sub_ack}) do
    decode_sub_ack(msg)
  end
  def decode_message(msg, h = %Msg.FixedHeader{message_type: :connect}) do
    decode_connect(msg)
  end
  def decode_message(<<_reserved :: bytes-size(1), status :: integer-size(8)>>,
                     h = %Msg.FixedHeader{message_type: :conn_ack}) do
    Spotmq.Msg.conn_ack(conn_ack_status(status))
  end

  def decode_publish(msg, h) do
    {topic, m1} = utf8(msg)
    # in m1 is the message id if qos = 1 or 2
    {msg_id, payload} = case h.qos do
      :fire_and_forget -> {0, m1}
      _   ->
        <<id :: unsigned-integer-size(16), content :: binary>> = m1
        {id, content}
    end
    ## create a publish message
    p = Spotmq.Msg.req_publish(topic, payload, h.qos)
    %Spotmq.Msg.ReqPublish{p | header: h, msg_id: msg_id}
  end

  def decode_unsubscribe(<<msg_id :: unsigned-integer-size(16), content :: binary>>) do
    topics = utf8_list(content)
    Spotmq.Msg.unsubscribe(topics, msg_id)
  end

  def decode_sub_ack(<<msg_id :: unsigned-integer-size(16), content :: binary>>) do
    granted_qos = qos_list(content)
    Spotmq.Msg.sub_ack(granted_qos, msg_id)
  end

  def decode_connect(<<0x00, 0x06, "MQIsdp", 0x03, flags :: size(8), keep_alive :: size(16), rest::binary>>) do
    <<user_flag :: size(1),
      pass_flag :: size(1),
      w_retain :: size(1),
      w_qos :: size(2),
      w_flag :: size(1),
      clean :: size(1),
      _ ::size(1) >> = <<flags>>

    {client_id, payload} = extract(1, utf8_list(rest))
    {will_topic, will_message, payload} = extract2(w_flag, payload)
    {user_name, payload} = extract(user_flag, payload)
    {password, payload} = extract(pass_flag, payload)

    alive = if (keep_alive == 0) do :infinity else :keep_alive end

    Spotmq.Msg.connection(client_id,
                          user_name,
                          password,
                          clean == 1,
                          alive,
                          w_flag == 1,
                          binary_to_qos(w_qos),
                          w_retain == 1,
                          will_topic,
                          will_message)
  end

  def decode_subscribe(<<msg_id :: unsigned-integer-size(16), payload :: binary>>, h) do
    topics = topics(payload)
    %Spotmq.Msg.Subscribe{ Spotmq.Msg.subscribe(topics, msg_id) | header: h}
    #	Spotmq.Msg.Subscribe.duplicate(h.duplicate == 1)
  end

  def topics(<<>>, acc) do
    Enum.reverse acc
  end

  def topics(payload, acc \\ []) do
    {topic, rest} = utf8(payload)
    <<qos :: size(8), r :: binary>> = rest
    topics(r, [{topic, binary_to_qos(qos)} | acc])
  end

  def extract(0, list) do
    {"", list}
  end
  def extract(1, list) do
    {hd(list), tl(list)}
  end

  def extract2(0, list) do
    {"", "", list}
  end
  def extract2(1, list) do
    {hd(list), hd(tl list), tl(tl list)}
  end

  def qos_list(<<>>, acc) do
    Enum.reverse acc
  end
  def qos_list(<<q :: size(8), rest :: binary>>, acc \\ []) do
    qos_list(rest, [binary_to_qos(q) | acc])
  end

  def get_msgid(<<id :: unsigned-integer-size(16)>>) do
    id
  end

  def utf8_list(<<>>, acc) do
    Enum.reverse acc
  end
  def utf8_list(content, acc \\ []) do
    {t, rest} = utf8(content)
    utf8_list(rest, [t | acc])
  end

  def utf8(<<length :: integer-unsigned-size(16), content :: bytes-size(length), rest :: binary>>) do
    {content, rest}
  end

  def utf8(nil) do
    {"", <<>>}
  end

  def binary_to_length(bin, count \\ 4, readByte_fun)
  def binary_to_length(_bin, count = 0, _readByte) do
    raise "Invalid length"
  end
  def binary_to_length(<<overflow :: size(1), len :: size(7)>>, count, readByte) do
    case overflow do
      1 ->
        {byte, nextByte} = readByte.()
        len + (binary_to_length(byte, count - 1, nextByte) <<< 7)
      0 -> len
    end
  end

  def binary_to_qos(bin) do
    case bin do
      0 -> :fire_and_forget
      1 -> :at_least_once
      2 -> :exactly_once
      3 -> :reserved
    end
  end

  def binary_to_msg_type(bin) do
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

  def conn_ack_status(bin) do
    case bin do
      0 -> :ok
      1 -> :unaccaptable_protocol_version
      2 -> :identifier_rejected
      3 -> :server_unavailable
      4 -> :bad_user
      5 -> :not_authorized
    end
  end

end
