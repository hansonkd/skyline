defmodule Spotmq.Msg.PublishReq do
  alias Spotmq.Msg.FixedHeader
  alias Spotmq.Msg.Decode.Utils

  defstruct topic: "",
            msg_id: 0,
            message: "",
            retain: false

  @doc "Creates a new publish request message."
	def create(topic, message, qos, msg_id, retain) do
		# length = byte_size(message) +
		#          byte_size(topic) + 2 + # with 16 bit size of topic
		#          2 # 16 bit message id
		%__MODULE__{topic: topic,
                message: message,
                msg_id: msg_id,
                retain: retain}
	end

  def decode_body(msg, h) do
    {topic, m1} = Utils.utf8(msg)
    # in m1 is the message id if qos = 1 or 2
    {msg_id, payload} = case h.qos do
      :fire_and_forget -> {0, m1}
      _   ->
        <<id :: unsigned-integer-size(16), content :: binary>> = m1
        {id, content}
    end
    ## create a publish message
    create(topic, payload, h.qos, msg_id, h.retain)
    # %Spotmq.Msg.ReqPublish{p | header: h, msg_id: msg_id}
  end


  def convert_to_delivery(sub_topic, qos, msg_id, dup, %__MODULE__{message: msg}) do
      Spotmq.Msg.PublishDelivery.create(
        sub_topic,
        msg,
        qos,
        msg_id,
        dup
      )
  end


end
