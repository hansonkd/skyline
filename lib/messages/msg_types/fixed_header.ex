defmodule Spotmq.Msg.FixedHeader do
  defstruct message_type: :reserved,
            duplicate: false,
            qos: :fire_and_forget,
            retain: false,
            length: 0

  def create(msg_type,
             dup,
             qos,
             retain,
             length) when
                is_atom(msg_type) and
                is_boolean(dup) and
                is_atom(qos) and
                is_boolean(retain) and
                is_integer(length) and
                length >= 0 do

    %__MODULE__{message_type: msg_type,
                 duplicate: dup,
                 qos: qos,
                 retain: retain,
                 length: length}
  end
end
