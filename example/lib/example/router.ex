defmodule Example.Router do
  use Skyline.Topic.Router
  
  pipeline :userpipeline do
    pipe Example.Pipe.UserGuard, []
  end
  
  def init(opts \\ nil) do
      Example.EventHandler.init
  end
  
  # Typically MQTT topics that start with $ are for admins, but not required.
  resource("$*admin", Admin)

  scope "user/:username/", alias: Example.Controller do
    pipe_through :userpipeline
    resource("location", LocationResource)
    # You can specify the publish and subscribe controllers seperately.
    # The can be two different modules or in one.
    publish("*other", UserNoMatchController)
    subscribe("*other", UserNoMatchController)
  end
  # This will catch everything else. The default resource controller will
  # allow all publish and subscribes.
  resource("*path", Skyline.Topic.Default.ResourceController)
end