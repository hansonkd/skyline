defmodule Skyline.AppConfig do
  defstruct auth: %Skyline.Auth.NoAuth{},
            topic_handlers: [],
            fall_back_topic_handler: nil #%Skyline.Topic.PrePublishHook.Default{}


  def default() do
    %__MODULE__{}
  end

  #@spec topic(__MODULE__.t, )
  def add_topic_handler(self, h) do

  end
end
