defmodule Spotmq.Msg.Connect do
  alias Spotmq.Msg.Decode.Utils

  defstruct client_id: "", # :: binary,
            user_name: "", # :: binary,
            password: "", # :: binary,
            keep_alive_ms:  :infinity, # or the keep-alive in milliseconds (=1000*mqtt-keep-alive)
            keep_alive_server_ms: :infinity, # or 1.5 * keep-alive in milliseconds (=1500*mqtt-keep-alive)
            last_will: false, # :: boolean,
            will_qos: :fire_and_forget, # :: Mqttex.qos_type,
            will_retain: false, # :: boolean,
            will_topic: "", # :: binary,
            will_message: "", # :: binary,
            clean_session: true # :: boolean,

  @doc """
	Creates a new connect message.
	"""
  def create(client_id,
              user_name,
              password,
              clean_session,
              keep_alive, # keep_alive_server \\ :infinity,
              keep_alive_server,
              last_will,
              will_qos,
              will_retain,
              will_topic,
              will_message) do

  	%__MODULE__{client_id: client_id,
                user_name: user_name,
                password: password,
  		          keep_alive_ms: keep_alive,
                keep_alive_server_ms: keep_alive_server,
                last_will: last_will,
                will_qos: will_qos,
  		          will_retain: will_retain,
                will_topic: will_topic,
  		          will_message: will_message,
                clean_session: clean_session
              }
  end
  def decode_body(<<0, 4, "MQTT",  4, flags :: size(8), keep_alive :: size(16), rest::binary>>, _hdr) do
    decode_common(flags, keep_alive, rest)
  end
  def decode_body(<<0x00, 0x06, "MQIsdp", 0x03, flags :: size(8), keep_alive :: size(16), rest::binary>>, _hdr) do
    decode_common(flags, keep_alive, rest)
  end
  def decode_common(flags, keep_alive, rest) do
    <<user_flag :: size(1),
      pass_flag :: size(1),
      w_retain :: size(1),
      w_qos :: size(2),
      w_flag :: size(1),
      clean :: size(1),
      _ ::size(1) >> = <<flags>>

    {client_id, payload} = extract(1, Utils.utf8_list(rest, []))
    {will_topic, will_message, payload} = extract2(w_flag, payload)
    {user_name, payload} = extract(user_flag, payload)
    {password, _payload} = extract(pass_flag, payload)

    {alive, alive_server} = if (keep_alive == 0) do
      {:infinity, :infinity}
    else
      {keep_alive * 1000, (keep_alive + 10) * 1000}
    end

    create(client_id,
        user_name,
        password,
        clean == 1,
        alive,
        alive_server,
        w_flag == 1,
        Utils.binary_to_qos(w_qos),
        w_retain == 1,
        will_topic,
        will_message)
  end

  defp extract(0, list) do
    {"", list}
  end
  defp extract(1, list) do
    {hd(list), tl(list)}
  end

  defp extract2(0, list) do
    {"", "", list}
  end
  defp extract2(1, list) do
    {hd(list), hd(tl list), tl(tl list)}
  end


end
