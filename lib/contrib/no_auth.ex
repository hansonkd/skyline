defmodule Skyline.Contrib.NoAuth do
  @moduledoc """
  An auth system that accepts all connections and returns a `nil` auth_info.
  """
  defstruct []

  def init() do
    %__MODULE__{}
  end
end

defimpl Skyline.Auth.Protocol, for: Skyline.Contrib.NoAuth do
  def new_connection(_self, _con_msg) do
    {:ok, nil}
  end
end
