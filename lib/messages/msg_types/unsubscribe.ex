defmodule Spotmq.Msg.Unsubscribe do
  defstruct topics: [],
            msg_id: nil

  alias Spotmq.Msg.Decode.Utils

  def create(topics, msg_id) do
    %__MODULE__{topics: topics, msg_id: msg_id}
  end

  def decode_body(<<msg_id :: unsigned-integer-size(16), content :: binary>>, _hdr) do
    topics = Utils.utf8_list(content, [])
    create(topics, msg_id)
  end
end
