defmodule Spotmq.Msg.Unsubscribe do
  @moduledoc """
  Unsubscribe

  http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718072
  """
  defstruct topics: [],
            msg_id: nil
  @type t :: %__MODULE__{msg_id: pos_integer, topics: [String.t]}
  @behaviour Spotmq.Msg.Decode
  
  alias Spotmq.Msg.Decode.Utils

  def new(topics, msg_id) do
    %__MODULE__{topics: topics, msg_id: msg_id}
  end

  @spec decode_body(binary, Spotmq.Msg.FixedHeader.t) :: __MODULE__.t
  def decode_body(<<msg_id :: unsigned-integer-size(16), content :: binary>>, _hdr) do
    topics = Utils.utf8_list(content, [])
    new(topics, msg_id)
  end
end
