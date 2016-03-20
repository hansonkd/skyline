defmodule Skyline.Msg.ConnAck do
  @moduledoc false
  #ConnAck MQTT Message
  #
  #http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718033

  @behaviour Skyline.Msg.Decode

  defstruct status: :ok
  @type t :: %__MODULE__{status: Skyline.conn_ack_type}

  @doc """
  Construct a new ConnAck.
  """
  @spec new(Skyline.conn_ack_type) :: __MODULE__.t
  def new(status) do
    %__MODULE__{status: status}
  end

  @spec decode_body(binary, Skyline.Msg.FixedHeader.t) :: __MODULE__.t
  def decode_body(<<_reserved :: bytes-size(1), status :: integer-size(8)>>, _h) do
    new(decode_status(status))
  end

  @doc """
  Decode binary into conn_ack_type.
  """
  @spec decode_status(binary) :: Skyline.conn_ack_type
  def decode_status(bin) do
    case bin do
      0 -> :ok
      1 -> :unaccaptable_protocol_version
      2 -> :identifier_rejected
      3 -> :server_unavailable
      4 -> :bad_user
      5 -> :not_authorized
    end
  end
end

defimpl Skyline.Msg.Encode, for: Skyline.Msg.ConnAck do
  alias Skyline.Msg.Encode.Utils

  def encode(msg) do
    <<Utils.msg_type_to_binary(:conn_ack) :: size(4),
      0 :: size(4),
      0x02,
      0x00,
      conn_ack_status(msg.status)>>

  end
  def conn_ack_status(atom) do
    case atom do
      :ok -> 0
      :unaccaptable_protocol_version -> 1
      :identifier_rejected -> 2
      :server_unavailable -> 3
      :bad_user -> 4
      :not_authorized -> 5
    end
  end
end
