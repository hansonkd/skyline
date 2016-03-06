defmodule Spotmq.Msg.PublishDelivery do
  defstruct subscription_topic: "", # We have to send back the subscription pattern that picked it up.
            msg_id: nil,
            message: "",
            duplicate: false,
            qos: :fire_and_forget


  def create(subscription_topic,
            message,
            qos,
            msg_id,
            duplicate) do

    %__MODULE__{
      subscription_topic: subscription_topic,
      message: message,
      qos: qos,
      msg_id: msg_id,
      duplicate: duplicate
    }

  end
end
defimpl Spotmq.Msg.Encode, for: Spotmq.Msg.PublishDelivery do
  alias Spotmq.Msg.Encode.Utils

  def encode(msg) do
    rest = Utils.utf8(msg.subscription_topic) <>
           Utils.msg_id(msg.msg_id) <>
           Utils.utf8(msg.message)

    header = Utils.encode_full_header(
       :publish,
       msg.duplicate,
       msg.qos,
       false,
       byte_size(rest)
    )
    ##IO.puts("\n\nEncode Publish #{inspect msg}\n\n")
    header <> rest
  end
end
