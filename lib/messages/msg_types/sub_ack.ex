defmodule Spotmq.Msg.SubAck do
  defstruct msg_id: nil,
            granted_qos: [] # :: [pos_integer],

  @spec create([Mqttex.qos_type], pos_integer) :: SubAck.t
  def create(granted_qos, msg_id) do
    %__MODULE__{msg_id: msg_id,
                granted_qos: granted_qos}
  end

  def decode_body(<<msg_id :: unsigned-integer-size(16), content :: binary>>, _hdr) do
    granted_qos = qos_list(content, [])
    create(granted_qos, msg_id)
  end

  def qos_list(<<>>, acc) do
    Enum.reverse acc
  end
  def qos_list(<<q :: size(8), rest :: binary>>, acc) do
    qos_list(rest, [Utils.binary_to_qos(q) | acc])
  end

end
defimpl Spotmq.Msg.Encode, for: Spotmq.Msg.SubAck do
  alias Spotmq.Msg.Encode.Utils

  def encode(msg) do

    #qos_bin = Utils.qos_binary(hd(msg.granted_qos))
    var_header = Utils.msg_id(msg.msg_id)
    payload = Enum.map_join(msg.granted_qos, "", &Utils.qos_binary/1) #>> #<< qos_bin :: size(16) >> # TODO:

    rest = var_header <> payload

    hdr = Utils.encode_full_header(
      :sub_ack,
      false,
      :fire_and_forget,
      false,
      byte_size(rest)
      )
    hdr <> rest
  end
end
