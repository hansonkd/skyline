defmodule SkylineApp do
  import Skyline.AppConfig

  def init() do
    %Skyline.AppConfig{
      auth: %AuthConfig{admins: ["admin"]},
      router: SkylineApp.Router
    }
  end
end
