defmodule Spotmq.Msg do
	defmodule Simple do
		@moduledoc """
		Defines all simple messages as structs. They contain at most a message id.
		"""
		defstruct msg_type: :reserved,
			        msg_id: 0

	end

	def pub_ack(msg_id) when is_integer(msg_id) do
    %Simple{msg_type: :pub_ack, msg_id: msg_id}
  end
	def pub_rec(msg_id) when is_integer(msg_id) do
    %Simple{msg_type: :pub_rec, msg_id: msg_id}
  end
	def pub_comp(msg_id) when is_integer(msg_id) do
    %Simple{msg_type: :pub_comp, msg_id: msg_id}
  end
	def ping_req() do
    %Simple{msg_type: :ping_req}
  end
	def ping_resp() do
    %Simple{msg_type: :ping_resp}
  end
	def disconnect() do
    %Simple{msg_type: :disconnect}
  end
	def unsub_ack(msg_id) when is_integer(msg_id) do
    %Simple{msg_type: :unsub_ack, msg_id: msg_id}
  end

	defmodule PubRel do
		defstruct msg_id: 0,
		          duplicate: false
	end

	def pub_rel(msg_id, duplicate \\ false) when is_integer(msg_id) do
    %PubRel{msg_id: msg_id, duplicate: duplicate}
  end

	defmodule ConnAck do
		defstruct status: :ok
	end

	def conn_ack(status \\ :ok) do
    %ConnAck{status: status}
  end

  defmodule FixedHeader do
    defstruct message_type: :reserved,
              duplicate: false,
              qos: :fire_and_forget,
              retain: false,
              length: 0
  end


	def fixed_header(msg_type \\ :reserved,
                   dup \\ false,
                   qos \\ :fire_and_forget,
			             retain \\ false,
                   length \\ 0) when
            		                is_atom(msg_type) and
                                is_boolean(dup) and
                                is_atom(qos) and
            		                is_boolean(retain) and
                                is_integer(length) and
                                length >= 0 do

		%FixedHeader{message_type: msg_type,
                 duplicate: dup,
			           qos: qos,
                 retain: retain,
                 length: length}
	end

  @doc "Client to Server Publish."
	defmodule ReqPublish do
		defstruct topic: "",
			        msg_id: 0,
			        message: "",
			        header: %FixedHeader{}

	end

  @doc "Server to Client Publish."
  defmodule ClientPublish do
    defstruct sub_topic: "", # We have to send back the subscription pattern that picked it up.
              msg_id: 0,
              message: "",
              header: %FixedHeader{},
              duplicate: false,
              qos: :fire_and_forget
  end

  def convert_to_client(sub_topic, qos, msg_id, dup, %ReqPublish{message: msg, header: hdr}) do
    %ClientPublish{
      sub_topic: sub_topic,
      message: msg,
      qos: hdr.qos,
      msg_id: msg_id,
      duplicate: dup
    }
  end

	@doc "Creates a new publish message. The message id is not set per default."
	def req_publish(topic, message, qos, msg_id \\ 0) do
		length = byte_size(message) +
		         byte_size(topic) + 2 + # with 16 bit size of topic
		         2 # 16 bit message id
		h = fixed_header(:publish, false, qos, false, length)
		%ReqPublish{topic: topic,
             message: message,
             msg_id: msg_id,
             header: h}
	end


	defmodule Unsubscribe do
		defstruct topics: [],
			        msg_id: 0,
			        header: %FixedHeader{}

		@doc "Sets the duplicate flag to `dup` in the message"
		def duplicate(%Unsubscribe{header: h} = m, dup \\ true) do
			new_h = %FixedHeader{h | duplicate: dup}
			%Unsubscribe{m | header: new_h}
		end

		@doc "Sets the message id"
		def msg_id(%Unsubscribe{} = m, id \\ 0) do
			%Unsubscribe{m | msg_id: id}
		end

	end

	@doc "Creates a new unsubscribe message. The message id is not set per default"
	@spec unsubscribe([binary], pos_integer) :: Unsubscribe.t
	def unsubscribe(topics, msg_id \\ 0) do
		length = 2 + #  16 bit message id
			(topics |> Enum.map(fn(t) -> byte_size(t) + 2 end) |> Enum.sum)
		h = fixed_header(:unsubscribe, false, :at_least_once, false, length)
		%Unsubscribe{topics: topics, msg_id: msg_id, header: h}
	end

	defmodule SubAck do
		defstruct msg_id: 0, # :: pos_integer,
			        granted_qos: [], # :: [pos_integer],
			        header: %FixedHeader{} # :: FixedHeader.t

		@doc "Sets the message id"
		def msg_id(%SubAck{} = m, id \\ 0) do
			%SubAck{m | msg_id: id}
		end

	end

	@doc "Creates a new sub_ack message. The message id is not set per default"
	@spec sub_ack([Mqttex.qos_type], pos_integer) :: SubAck.t
	def sub_ack(granted_qos, msg_id \\ 0) do
		length = 2 + # 16 bit message id
			length(granted_qos) # number of bytes per qos

		h = fixed_header(:sub_ack, false, :fire_and_forget, false, length)

    %SubAck{msg_id: msg_id,
            granted_qos: granted_qos,
            header: h }
	end

	defmodule Subscribe do
		defstruct msg_id: 0, # :: pos_integer,
			        header: %FixedHeader{}, # :: FixedHeader.t,
			        topics: [{"", :fire_and_forget}] # :: [{binary, Mqttex.qos_type}]

		@doc "Sets the message id"
		def msg_id(%Subscribe{} = m, id \\ 0) do
			%Subscribe{m | msg_id: id}
		end

		@doc "Sets the duplicate flag to `dup` in the message"
		def duplicate(%Subscribe{header: h} = m, dup \\ true) do
			new_h = %FixedHeader{h | duplicate: dup}
			%Subscribe{m | header: new_h}
		end

	end

	def subscribe(topics, msg_id \\ 0) when is_integer(msg_id) do
		length = 2 + # 16 bit message id
			(topics |> Enum.map(fn({t,q}) -> byte_size(t) + 3 # + 16 bit length + 1 byte qos
				end) |> Enum.sum)
		h = fixed_header(:subscribe, false, :at_least_once, false, length)
		%Subscribe{msg_id: msg_id, topics: topics, header: h}
	end


	defmodule Connection do

		defstruct client_id: "", # :: binary,
      			  user_name: "", # :: binary,
      			  password: "", # :: binary,
      			  keep_alive:  :infinity, # or the keep-alive in milliseconds (=1000*mqtt-keep-alive)
      			  # keep_alive_server: :infinity, # or 1.5 * keep-alive in milliseconds (=1500*mqtt-keep-alive)
      			  last_will: false, # :: boolean,
      			  will_qos: :fire_and_forget, # :: Mqttex.qos_type,
      			  will_retain: false, # :: boolean,
      			  will_topic: "", # :: binary,
      			  will_message: "", # :: binary,
      			  clean_session: true, # :: boolean,
      			  header: %FixedHeader{} # :: FixedHeader.t
	end

	@doc """
	Creates a new connect message.
	"""
	def connection(client_id,
                user_name,
                password,
                clean_session,
                keep_alive \\ :infinity, # keep_alive_server \\ :infinity,
			          last_will \\ false,
                will_qos \\ :fire_and_forget,
                will_retain \\ false,
                will_topic \\ "",
                will_message \\ "") do

		length = 12 +  # variable header size
			byte_size(client_id) + 2 +
			optional_size(user_name) +
			optional_size(password) +
			if (last_will) do
				byte_size(will_topic) + 2 + byte_size(will_message) + 2
			else
        0
      end

		h = fixed_header(:connect, false, :fire_and_forget, false, length)
		%Connection{client_id: client_id,
                user_name: user_name,
                password: password,
			          keep_alive: keep_alive,
                last_will: last_will,
                will_qos: will_qos,
			          will_retain: will_retain,
                will_topic: will_topic,
			          will_message: will_message,
                clean_session: clean_session,
                header: h }
	end

	def optional_size(""), do: 0
	def optional_size(bytes), do: 2 + byte_size(bytes)

end
