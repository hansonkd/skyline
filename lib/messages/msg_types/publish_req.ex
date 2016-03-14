defmodule Spotmq.Msg.PublishReq do
  @moduledoc """
  PingReq - Client -> Broker Publish

  http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718037
  """
  defstruct topic: "",
            msg_id: nil,
            message: "",
            qos: nil,
            retain: false
  @type t :: %__MODULE__{topic: String.t, msg_id: pos_integer, message: String.t, qos: SpotApp.qos_type, retain: boolean}
  @behaviour Spotmq.Msg.Decode

  alias Spotmq.Msg.FixedHeader
  alias Spotmq.Msg.Decode.Utils


  @doc "Creates a new publish request message."
	def new(topic, message, qos, msg_id, retain) do
		%__MODULE__{topic: topic,
                message: message,
                msg_id: msg_id,
                qos: qos,
                retain: retain}
	end

  @spec decode_body(binary, Spotmq.Msg.FixedHeader.t) :: __MODULE__.t
  def decode_body(msg, h) do
    {topic, m1} = Utils.utf8(msg)
    # in m1 is the message id if qos = 1 or 2
    {msg_id, payload} = case h.qos do
      :fire_and_forget -> {0, m1}
      _   ->
        <<id :: unsigned-integer-size(16), content :: binary>> = m1
        {id, content}
    end
    new(topic, payload, h.qos, msg_id, h.retain)
  end

  @doc "Convert a PublishReq to a PublishDelivery"
  @spec convert_to_delivery(binary, SpotApp.qos_type, pos_integer, boolean, __MODULE__.t) :: Spotmq.Msg.PublishDelivery.t
  def convert_to_delivery(sub_topic, qos, msg_id, dup, %__MODULE__{message: msg}) do
      Spotmq.Msg.PublishDelivery.new(
        sub_topic,
        msg,
        qos,
        msg_id,
        dup
      )
  end


end
