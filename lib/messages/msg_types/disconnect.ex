defmodule Spotmq.Msg.Disconnect do
  @moduledoc """
  Disconnect Message

  http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718090
  """
  @behaviour Spotmq.Msg.Decode
  defstruct []
  @type t :: %__MODULE__{}

  alias Spotmq.Msg.FixedHeader

  @doc """
  Creates a new Discnnect.
  """
  @spec new() :: __MODULE__.t
  def new() do
    %__MODULE__{}
  end

  @spec decode_body(binary, Spotmq.Msg.FixedHeader.t) :: __MODULE__.t
  def decode_body(<<>>, %FixedHeader{length: 0}) do
    new()
  end
  def decode_body(_msg, _hdr) do
    # Raise an error about needing 0 length
  end
end
defimpl Spotmq.Msg.Encode, for: Spotmq.Msg.Disconnect do
  alias Spotmq.Msg.Encode.Utils

  def encode(_msg) do
    Utils.encode_basic(:disconnect)
  end
end
