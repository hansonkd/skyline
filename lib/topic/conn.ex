defmodule Skyline.Topic.Conn do
  @moduledoc """
  Holds the message and auth information to be passed to pipes and controllers.
  
  The auth_info you returned in your auth config can be retrieved and manipulated for subsequenet messages:
      def some_pipe(%Conn{auth_info: MyAuth{username: username} = auth} = conn, _opts) do
          Logger.debug "Hi, \#{username}, you are now SomeOtherUser!"
          %{conn | message: %{msg | body <> " woah"}, auth_info: %{auth | username: "SomeOtherUser"}}
      end
  Or filter people out
      def some_pipe(%Conn{auth_info: MyAuth{username: username}} = conn, _opts) do
          if username == "admin" do
              conn
          else
              {:close_connection, "Not cool enough"}
          end
      end
  
  To change the body of a Publish message, you can might do
  
      def exciting_pipe(%Conn{message: %PubReq{message: body} = msg} = conn, _opts) do
          %{conn | message: %{msg | body <> "!!!!!!!"}}
      end

  `Conn.topic` and `Conn.qos` are the requested topic and Qos of the message. These can be manipulated in either publish
  or subscribe events. You can transform the topic name or downgrade the Qos handling for the message. Changing the topic will
  not affect routing.
  
  During a Publish event, the message will have a `%PublishReq{topic: String}` param. Changing this does nothing.
  If you want to affect what topic the
  message is sent to, change `topic` in `%Conn{}`. Before Skyline dispatches the message, it will take whatever topic is
  in `%Conn{}` and put it on the message, although the topic the client recieves is the subscription topic and not the topic 
  the message was published to, accoring to the MQTT spec. If the client subscribes to `something/+/woah` and a message was 
  sent to `something/cool/woah`, the publish message the client recieves is `something/+/woah`.
  
  During a Subscribe event, the message will have `%Subscribe{topics: [String]}`. Manipulating the message will not
  have an effect on how Skyline handles the returned `%Conn{}`. This is because Skyline iterates through all the topics
  in a message and makes a call for each one. Therefore to change what topic is subscribed, alter `topic` in `%Conn{}`.
  When Skyline creates the subscriber process it will look at `conn.topic`.
  """
  defstruct [:message, :qos, :auth_info, :topic, :private, :path_info, :action, :params]
  @type t :: %__MODULE__{topic: String.t, message: Skyline.skyline_msg, qos: Skyline.qos_type,
                         auth_info: term, path_info: [String.t], }
  
  alias Skyline.Topic.Conn
  alias Skyline.Client
  
  @doc false
  @spec put_private(t, atom, term) :: t
  def put_private(%Conn{private: private} = conn, key, value) when is_atom(key) do
    %{conn | private: Map.put(private, key, value)}
  end

  @doc false
  def conn(topic, qos, message, action, %Client{sess_pid: sess_pid, 
                                                client_id: client_id,
                                                auth_info: auth_info}) do
    %__MODULE__{
      message: message,
      topic: topic,
      qos: qos,
      auth_info: auth_info,
      action: action,
      params: %{},
      private: %{sess_pid: sess_pid, client_id: client_id},
      path_info: Skyline.Topic.Pipe.Utils.split(topic)
    }
  end
end
