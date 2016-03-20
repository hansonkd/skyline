defmodule Skyline.Msg.PingReq do
  @moduledoc """
  PingReq MQTT Message

  http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718081
  """
  defstruct []
  @type t :: %__MODULE__{}
  @behaviour Skyline.Msg.Decode

  alias Skyline.Msg.FixedHeader

  @doc """
  Creates a new PingReq.
  """
  @spec new() :: __MODULE__.t
  def new() do
    %__MODULE__{}
  end

  @spec decode_body(binary, Skyline.Msg.FixedHeader.t) :: __MODULE__.t
  def decode_body(<<>>, %FixedHeader{length: 0}) do
    new()
  end
  def decode_body(_msg, _hdr) do
    # Raise an error about needing 0 length
  end
end
defimpl Skyline.Msg.Encode, for: Skyline.Msg.PingReq do
  alias Skyline.Msg.Encode.Utils

  def encode(_msg) do
    Utils.encode_basic(:ping_req)
  end
end
