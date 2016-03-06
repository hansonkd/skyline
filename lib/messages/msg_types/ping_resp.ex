defmodule Spotmq.Msg.PingResp do
  @moduledoc """
  Ping Req
  """
  defstruct []

  alias Spotmq.Msg.FixedHeader

  def create() do
    %__MODULE__{}
  end
  def decode_body(<<>>, %FixedHeader{length: 0}) do
    create()
  end
  def decode_body(_msg, _hdr) do
    # Raise an error about needing 0 length
  end
end
defimpl Spotmq.Msg.Encode, for: Spotmq.Msg.PingResp do
  alias Spotmq.Msg.Encode.Utils

  def encode(_msg) do
    Utils.encode_basic(:ping_resp)
  end
end
