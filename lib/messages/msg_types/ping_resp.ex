defmodule Skiline.Msg.PingResp do
  @moduledoc """
  PingResp

  http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718086
  """
  defstruct []
  @type t :: %__MODULE__{}
  @behaviour Skiline.Msg.Decode
  
  alias Skiline.Msg.FixedHeader

  @doc """
  Creates a new PingResp.
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
defimpl Skiline.Msg.Encode, for: Skiline.Msg.PingResp do
  alias Skiline.Msg.Encode.Utils

  def encode(_msg) do
    Utils.encode_basic(:ping_resp)
  end
end
