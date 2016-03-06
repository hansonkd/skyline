defprotocol Spotmq.Msg.Decode do
  # Make this a behaviour
  alias Spotmq.Msg.FixedHeader
  @callback decode_body(binary, FixedHeader.t) :: any
end
