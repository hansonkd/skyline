defmodule Skyline.Msg.PubRec do
  @moduledoc false
  
  # PingRec MQTT Message
  #
  # http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718048

  defstruct msg_id: nil
  @type t :: %__MODULE__{msg_id: pos_integer}
  @behaviour Skyline.Msg.Decode

  alias Skyline.Msg.Decode.Utils

  @doc "New PubRec Message"
  @spec new(pos_integer) :: __MODULE__.t
  def new(msg_id) do
    %__MODULE__{msg_id: msg_id}
  end

  @spec decode_body(binary, Skyline.Msg.FixedHeader.t) :: __MODULE__.t
  def decode_body(msg, _hdr) do
    new(Utils.get_msgid(msg))
  end
end

defimpl Skyline.Msg.Encode, for: Skyline.Msg.PubRec do
  alias Skyline.Msg.Encode.Utils

  def encode(%Skyline.Msg.PubRec{msg_id: msg_id}) do
    Utils.basic_with_msg_id(:pub_rec, msg_id)
  end
end
