defmodule Skyline.AppConfig do
  defstruct auth: nil,
            router: nil

  def default() do
    %__MODULE__{}
  end

  #@spec topic(__MODULE__.t, )
  def add_topic_handler(self, h) do

  end

end
