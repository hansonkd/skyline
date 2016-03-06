defmodule Spotmq.Msg.Subscribe do

  alias Spotmq.Msg.Decode.Utils

  defstruct msg_id: nil,
            topics: [{"", :fire_and_forget}] # :: [{binary, Mqttex.qos_type}]

  def create(topics, msg_id) when is_integer(msg_id) do
		# length = 2 + # 16 bit message id
		# 	(topics |> Enum.map(fn({t,q}) -> byte_size(t) + 3 # + 16 bit length + 1 byte qos
		# 		end) |> Enum.sum)
		# h = FixedHeader.create(:subscribe, false, :at_least_once, false, length)
		%__MODULE__{msg_id: msg_id, topics: topics}
	end

  def decode_body(<<msg_id :: unsigned-integer-size(16), payload :: binary>>, _hdr) do
    topics = topics(payload)
    create(topics, msg_id)
  end

  def topics(<<>>, acc) do
    Enum.reverse acc
  end

  def topics(payload, acc \\ []) do
    {topic, rest} = Utils.utf8(payload)
    <<qos :: size(8), r :: binary>> = rest
    topics(r, [{topic, Utils.binary_to_qos(qos)} | acc])
  end
end
