defmodule Spotmq.Msg.Decode do
  @callback decode_body(binary, Spotmq.Msg.FixedHeader.t) :: any
end
