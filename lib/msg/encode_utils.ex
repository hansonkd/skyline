defmodule Skyline.Msg.Encode.Utils do
    @moduledoc false

    # Common methods for encoding

    
    use Bitwise

    @doc "Encode messages that don't have attributes"
    @spec encode_basic(Skyline.empty_msg) :: binary
    def encode_basic(msg_type) when msg_type in [:ping_req, :ping_resp, :disconnect] do
        <<msg_type_to_binary(msg_type) :: size(4), 0 :: size(4), 0x00>>
    end

    @doc "Encode messages that have a message_id"
    @spec basic_with_msg_id(Skyline.msg_with_id, pos_integer) :: binary
    def basic_with_msg_id(msg_type, msg_id) when msg_type in [:pub_ack, :pub_rec, :pub_comp, :unsub_ack] do
        <<msg_type_to_binary(msg_type) :: size(4), 0 :: size(4), 0x02, msg_id(msg_id) :: binary>>
    end

    @doc "Encode all parts of the header"
    @spec encode_full_header(Skyline.msg_type, boolean, Skyline.qos_type, boolean, pos_integer) :: binary
    def encode_full_header(message_type,
                           duplicate,
                           qos,
                           retain,
                           length) do
      <<msg_type_to_binary(message_type) :: size(4),
        boolean_to_binary(duplicate) :: bits,
        qos_binary(qos) :: size(2),
        boolean_to_binary(retain) :: bits >>
        # Second Byte
        <> encode_length(length)
    end

    @doc "Converts the atoms to binary message types"
    @spec msg_type_to_binary(Skyline.msg_type) :: pos_integer
    def msg_type_to_binary(atom) do
      case atom do
        :reserved -> 0
        :connect -> 1
        :conn_ack -> 2
        :publish -> 3
        :pub_ack -> 4
        :pub_rec -> 5
        :pub_rel -> 6
        :pub_comp -> 7
        :subscribe -> 8
        :sub_ack -> 9
        :unsubscribe -> 10
        :unsub_ack -> 11
        :ping_req -> 12
        :ping_resp -> 13
        :disconnect -> 14
      end
    end

    @spec utf8(String.t) :: binary
  	def utf8(str) do
      <<byte_size(str) :: size(16) >> <> str
    end

    @spec msg_id(pos_integer) :: binary
  	def msg_id(id) when is_integer(id) do
      <<id :: size(16)>>
    end

    @spec keep_alive(Skyline.keep_alive) :: binary
  	def keep_alive(n) do
      case n do
        :infinity -> <<0 :: size(16)>>
        _ -> <<n :: big-size(16)>>
      end
    end

    @doc """
    Encode length as defined in
    http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718023
    """
    @spec encode_length(pos_integer) :: binary
  	def encode_length(0) do
      <<0, 0>>
    end
  	def encode_length(l) when l <= 268_435_455 do
      encode_length(l, <<>>)
    end
  	defp encode_length(0, acc) do
      acc
    end
  	defp encode_length(l, acc) do
  		digit = l &&& 0x7f # mod 128
  		new_l = l >>> 7 # div 128
  		if new_l > 0 do
  			# add high bit since there is more to come
  			encode_length(new_l, acc <> <<digit ||| 0x80>>)
  		else
  			encode_length(new_l, acc <> <<digit>>)
  		end
  	end

  	@doc "Converts boolean to bit"
    @spec boolean_to_binary(boolean) :: binary
  	def boolean_to_binary(bool) do
      case bool do
        true -> <<1 :: size(1)>>
        false -> <<0 :: size(1)>>
      end
    end

    @doc "Converts atoms the binary qos"
    @spec qos_binary(Skyline.qos_type) :: pos_integer
    def qos_binary(atom) do
      case atom do
        :fire_and_forget -> 0
        :at_least_once -> 1
        :exactly_once -> 2
        :reserved -> 3
      end
    end

end
