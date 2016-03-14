defmodule Skiline.Msg.Decode do
  @callback decode_body(binary, Skiline.Msg.FixedHeader.t) :: any
end
