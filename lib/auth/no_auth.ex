defmodule Skyline.Auth.NoAuth do
  defstruct []

  def init() do
    %__MODULE__{}
  end
end

defimpl Skyline.Auth.Protocol, for: Skyline.Auth.NoAuth do
  def new_connection(self, con_msg) do
    {:ok, nil}
  end
end
