defmodule Skyline.Topic.Conn do
  defstruct [:message, :qos, :client, :topic, :private, :path_info, :method, :params]

  alias Skyline.Topic.Conn
  #@spec put_private(t, atom, term) :: t
  def put_private(%Conn{private: private} = conn, key, value) when is_atom(key) do
    %{conn | private: Map.put(private, key, value)}
  end

  def conn(topic, qos, message, client, method) do
    %__MODULE__{
      message: message,
      topic: topic,
      qos: qos,
      client: client,
      method: method,
      params: %{},
      private: %{},
      path_info: Skyline.Topic.Pipe.Utils.split(topic)
    }
  end
end
