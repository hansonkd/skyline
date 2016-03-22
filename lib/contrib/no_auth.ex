defmodule Skyline.Contrib.NoAuth do
  @moduledoc """
  An auth system that accepts all connections and returns a `nil` auth_info.
  """
  defstruct []

  def init() do
    %__MODULE__{}
  end
  
  def new_connection(_con_msg, opts) do
    {:ok, opts}
  end
  
end
