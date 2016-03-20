defmodule Skyline.Msg.PublishDelivery do
  @moduledoc false

  # Publish MQTT Message | Broker -> Client Publish
  #
  # http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718037
  
  defstruct subscription_topic: "", # We have to send back the subscription pattern that picked it up.
            msg_id: nil,
            message: "",
            duplicate: false,
            qos: :fire_and_forget
  @type t :: %__MODULE__{subscription_topic: String.t, msg_id: pos_integer, message: String.t, qos: Skyline.qos_type, duplicate: boolean}

  def new(subscription_topic,
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
defimpl Skyline.Msg.Encode, for: Skyline.Msg.PublishDelivery do
  alias Skyline.Msg.Encode.Utils

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
