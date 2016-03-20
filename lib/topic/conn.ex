defmodule Skyline.Topic.Conn do
  defstruct [:message, :client, :topic, :private, :path_info, :method, :params]

  alias Skyline.Topic.Conn
  #@spec put_private(t, atom, term) :: t
  def put_private(%Conn{private: private} = conn, key, value) when is_atom(key) do
    %{conn | private: Map.put(private, key, value)}
  end

end
