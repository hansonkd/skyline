defmodule Skyline.Msg.Decode do
  @moduledoc false
  
  @callback decode_body(binary, Skyline.Msg.FixedHeader.t) :: Skyline.skyline_msg
end
