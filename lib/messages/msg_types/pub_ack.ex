defmodule Spotmq.Msg.PubAck do
  defstruct msg_id: nil

  alias Spotmq.Msg.Decode.Utils

  def create(msg_id) do
    %__MODULE__{msg_id: msg_id}
  end

  def decode_body(msg, _hdr) do
    create(Utils.get_msgid(msg))
  end

end

defimpl Spotmq.Msg.Encode, for: Spotmq.Msg.PubAck do
  alias Spotmq.Msg.Encode.Utils

  def encode(%Spotmq.Msg.PubAck{msg_id: msg_id}) do
    Utils.basic_with_msg_id(:pub_ack, msg_id)
  end
end
