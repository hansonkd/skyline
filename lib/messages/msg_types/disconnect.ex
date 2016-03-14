defmodule Skiline.Msg.Disconnect do
  @moduledoc """
  Disconnect Message

  http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718090
  """
  defstruct []
  @type t :: %__MODULE__{}
  @behaviour Skiline.Msg.Decode

  alias Skiline.Msg.FixedHeader

  @doc """
  Creates a new Discnnect.
  """
  @spec new() :: __MODULE__.t
  def new() do
    %__MODULE__{}
  end

  @spec decode_body(binary, Skiline.Msg.FixedHeader.t) :: __MODULE__.t
  def decode_body(<<>>, %FixedHeader{length: 0}) do
    new()
  end
  def decode_body(_msg, _hdr) do
    # Raise an error about needing 0 length
  end
end
defimpl Skiline.Msg.Encode, for: Skiline.Msg.Disconnect do
  alias Skiline.Msg.Encode.Utils

  def encode(_msg) do
    Utils.encode_basic(:disconnect)
  end
end
