defmodule Skyline.Msg.Cleanup do
  @moduledoc """
  Unofficial Message used Internally by skyline to trigger cleanups.
  """
  defstruct topic: nil
  @type t :: %__MODULE__{topic: String.t}
  @behaviour Skyline.Msg.Decode

  @spec new(String.t) :: Cleanup.t
  def new(topic) do
    %__MODULE__{topic: topic}
  end
end
