defmodule Example.Router do
  use Skyline.Topic.Router

  pipeline :userpipeline do
    pipe Example.Pipe.UserGuard, []
  end

  scope "", alias: Example.Controller do
    resource("$*admin", Admin)

    scope "user/:username/" do
      pipe_through :userpipeline

      resource("location", LocationResource)

      # You can specify the publish and subscribe controllers seperately.
      # The can be two different modules or in one.
      publish("*other", UserNoMatchController)
      subscribe("*other", UserNoMatchController)
    end
  end
  
  # This will catch everything else. The default resource controller will
  # allow all publish and subscribes.
  scope "", alias: Skyline.Topic.Default do
    resource("*path", ResourceController)
  end
end
