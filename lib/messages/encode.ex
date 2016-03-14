defprotocol Skyline.Msg.Encode do
  @moduledoc """
  Protocol for converting messages into binary
  """
  @spec encode(Skyline.Skyline_msg) :: binary
  def encode(msg)
end
