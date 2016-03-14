defmodule Spotmq.Msg.PubComp do
  @moduledoc """
  PubComp

  http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718058
  """
  defstruct msg_id: nil
  @type t :: %__MODULE__{msg_id: pos_integer}

  alias Spotmq.Msg.Decode.Utils
  @behaviour Spotmq.Msg.Decode

  @doc "New PubComp Message"
  @spec new(pos_integer) :: __MODULE__.t
  def new(msg_id) do
    %__MODULE__{msg_id: msg_id}
  end

  @spec decode_body(binary, Spotmq.Msg.FixedHeader.t) :: __MODULE__.t
  def decode_body(msg, _hdr) do
    new(Utils.get_msgid(msg))
  end
end

defimpl Spotmq.Msg.Encode, for: Spotmq.Msg.PubComp do
  alias Spotmq.Msg.Encode.Utils

  def encode(%Spotmq.Msg.PubComp{msg_id: msg_id}) do
    Utils.basic_with_msg_id(:pub_comp, msg_id)
  end
end
