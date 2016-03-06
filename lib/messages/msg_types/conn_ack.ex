defmodule Spotmq.Msg.ConnAck do
  defstruct status: :ok

  def create(status) do
    %__MODULE__{status: status}
  end

  def decode_body(<<_reserved :: bytes-size(1), status :: integer-size(8)>>, h) do
    create(decode_status(status))
  end

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

defimpl Spotmq.Msg.Encode, for: Spotmq.Msg.ConnAck do
  alias Spotmq.Msg.Encode.Utils

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
