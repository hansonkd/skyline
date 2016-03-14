defmodule Skyline.Msg.Subscribe do
  @moduledoc """
  Subscribe MQTT Message

  http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718063
  """
  defstruct msg_id: nil,
            topics: []
  @type t :: %__MODULE__{msg_id: pos_integer, topics: [{String.t, Skyline.qos_type}]}
  @behaviour Skyline.Msg.Decode

  alias Skyline.Msg.Decode.Utils

  def new(topics, msg_id) when is_integer(msg_id) do
		%__MODULE__{msg_id: msg_id, topics: topics}
	end

  @spec decode_body(binary, Skyline.Msg.FixedHeader.t) :: __MODULE__.t
  def decode_body(<<msg_id :: unsigned-integer-size(16), payload :: binary>>, _hdr) do
    topics = topics(payload)
    new(topics, msg_id)
  end

  @spec topics(binary, [{binary, Skyline.qos_type}]) :: [{binary, Skyline.qos_type}]
  defp topics(<<>>, acc) do
    Enum.reverse acc
  end
  defp topics(payload, acc \\ []) do
    {topic, rest} = Utils.utf8(payload)
    <<qos :: size(8), r :: binary>> = rest
    topics(r, [{topic, Utils.binary_to_qos(qos)} | acc])
  end
end
