defmodule Skyline.Contrib.PassThroughRouter do
  @moduledoc "A router that lets all messages pass-through."
  
  alias Skyline.Topic.Conn
  
  @behaviour Skyline.Topic.Pipe

  def init(opts \\ nil) do
    opts
  end

  def call(%Conn{} = conn, _opts) do
      conn
  end

end
