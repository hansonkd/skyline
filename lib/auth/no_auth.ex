defmodule Skyline.Auth.NoAuth do
  defstruct []
end

defimpl Skyline.Auth.Protocol, for: Skyline.Auth.NoAuth do
  def new_connection(self, con_msg) do
    {:ok, nil}
  end
end
