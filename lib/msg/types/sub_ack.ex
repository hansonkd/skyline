defmodule Skyline.Msg.SubAck do
  @moduledoc false

  # SubAck MQTT Message
  #
  # http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718068
  
  defstruct msg_id: nil,
            granted_qos: []
  @type t :: %__MODULE__{msg_id: pos_integer, granted_qos: [Skyline.qos_type]}
  @behaviour Skyline.Msg.Decode

  @spec new([Mqttex.qos_type], pos_integer) :: SubAck.t
  def new(granted_qos, msg_id) do
    %__MODULE__{msg_id: msg_id,
                granted_qos: granted_qos}
  end

  @spec decode_body(binary, Skyline.Msg.FixedHeader.t) :: __MODULE__.t
  def decode_body(<<msg_id :: unsigned-integer-size(16), content :: binary>>, _hdr) do
    granted_qos = qos_list(content, [])
    new(granted_qos, msg_id)
  end

  @spec qos_list(binary, [Skyline.qos_type]):: [Skyline.qos_type]
  defp qos_list(<<>>, acc) do
    Enum.reverse acc
  end
  defp qos_list(<<q :: size(8), rest :: binary>>, acc) do
    qos_list(rest, [Utils.binary_to_qos(q) | acc])
  end

end
defimpl Skyline.Msg.Encode, for: Skyline.Msg.SubAck do
  alias Skyline.Msg.Encode.Utils

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
