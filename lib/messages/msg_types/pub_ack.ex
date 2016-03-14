defmodule Spotmq.Msg.PubAck do
  @moduledoc """
  PubAck

  http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718043
  """
  defstruct msg_id: nil
  @type t :: %__MODULE__{msg_id: pos_integer}
  @behaviour Spotmq.Msg.Decode
  
  alias Spotmq.Msg.Decode.Utils

  @doc "New PubAck Message"
  @spec new(pos_integer) :: __MODULE__.t
  def new(msg_id) do
    %__MODULE__{msg_id: msg_id}
  end

  @spec decode_body(binary, Spotmq.Msg.FixedHeader.t) :: __MODULE__.t
  def decode_body(msg, _hdr) do
    new(Utils.get_msgid(msg))
  end

end

defimpl Spotmq.Msg.Encode, for: Spotmq.Msg.PubAck do
  alias Spotmq.Msg.Encode.Utils

  def encode(%Spotmq.Msg.PubAck{msg_id: msg_id}) do
    Utils.basic_with_msg_id(:pub_ack, msg_id)
  end
end
