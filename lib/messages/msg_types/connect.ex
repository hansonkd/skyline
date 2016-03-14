defmodule Spotmq.Msg.Connect do
  @moduledoc """
  Connect Message

  http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718028
  """
  defstruct client_id: "",
            user_name: "",
            password: "",
            keep_alive_ms:  :infinity,
            keep_alive_server_ms: :infinity,
            last_will: false,
            will_qos: :fire_and_forget,
            will_retain: false,
            will_topic: "",
            will_message: "",
            clean_session: true
  @type t :: %__MODULE__{client_id: String.t,
                         user_name: String.t,
                         password: String.t,
                         keep_alive_ms: SpotApp.keep_alive,
                         keep_alive_server_ms: SpotApp.keep_alive,
                         last_will: boolean,
                         will_qos: SpotApp.qos_type,
                         will_retain: boolean,
                         will_topic: String.t,
                         will_message: String.t,
                         clean_session: boolean
                       }
  @behaviour Spotmq.Msg.Decode

  alias Spotmq.Msg.Decode.Utils

  @doc """
	Creates a new Connect.
	"""
  @spec new(binary, binary, binary, boolean, SpotApp.keep_alive, SpotApp.keep_alive, boolean, SpotApp.qos_type, boolean, binary, binary) :: __MODULE__.t
  def new(client_id,
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

  @spec decode_body(binary, Spotmq.Msg.FixedHeader.t) :: __MODULE__.t
  def decode_body(<<0, 4, "MQTT",  4, flags :: size(8), keep_alive :: size(16), rest::binary>>, _hdr) do
    decode_common(flags, keep_alive, rest)
  end
  def decode_body(<<0x00, 0x06, "MQIsdp", 0x03, flags :: size(8), keep_alive :: size(16), rest::binary>>, _hdr) do
    decode_common(flags, keep_alive, rest)
  end
  defp decode_common(flags, keep_alive, rest) do
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

    new(client_id,
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
