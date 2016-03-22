# SkylineMQ 

A fast, hackable MQTT message broker with a dash of Pheonix. 

## Goal

When looking around at a MQTT packages, almost all of them were static packages that you configure with hooks. Skyline aims to be a framework for building brokers, not configuring brokers.

It is also a mechanism for plugging into outside events, like building an Adapter for Phoenix Channels (not implemented yet).

## MQTT in a nutshell

MQTT is a lightweight machine-to-machine messaging protocol. It is pretty flexible and leaves a lot of the implementation up to the broker. Clients publish and subscribe to topics and choose their Quality of Service.

Subscribing to a topic is done by patterns that can contain wildcards. `+` is a wildcard for a level. `#` is a wildcard for anything following it

Consider the subscriptions:

1. `device/thermostat/temperature`
1. `device/+/temperature`
1. `device/#`
1. `device/thermostat/#`

Sending a message with the topic `device/thermostat/temperature` will match all of the subscriptions.

`device/computer/temperature` will match 2 and 3.

`device/thermostat/termerature/kelvin` will match 3 and 4.


## How it works

All Publish and Subscribe messages are passed through a Topic Router. With a router, you can change the topic name, manipulate auth information or change the message contents.

Message transformations are accomplished in a style similar to Phoenix:

         defmodule MyApp.Router do
           use Skyline.Topic.Router
           pipeline :session do
             pipe :fetch_session
           end
           scope "" do
             pipe_through :session
           end
         end

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed as:

  1. Add Skyline to your list of dependencies in `mix.exs`:

        def deps do
          [{:skyline, "~> 0.0.1"}]
        end

  2. Ensure Skyline is started before your application:

        def application do
          [applications: [:skyline]]
        end

  3. Configure skyline in config.exs (this configuration will make a standard, no-frills broker):

        config :skyline,
            port: 8000,
            auth_module: Skyline.Contrib.NoAuth,
            router_module: Skyline.Contrib.PassThroughRouter

  4. Initiate the Mnesia databases:

        mix skyline.create_db


## Creating an AuthHandler

The auth_module requires you implement the `Skyline.Auth.AuthHandler` behaviour. It needs an `init/0` method that initializes the options and a `new_connection/2` method that either denies the connection by passing back one of the MQTT auth connack statuses or the `auth_info` that will be passed to every call to the Router.

        defmodule Example.User do
          @moduledoc "Info about authenticated user passed in Skyline.Conn"
          defstruct username: nil, is_admin: false
        end
        defmodule Example.DumbAuth do
          defstruct admins: []
          @behaviour Skyline.Auth.AuthHandler
          def init() do
              %Example.DumbAuth{admins: ["admin"]}
          end
          def new_connection(%Connect{user_name: username}, %AuthConfig{admins: admins}) do
            if username && String.length(username) > 0 do
              {:ok, %Example.User{username: username, is_admin: username in admins}}
            else
              IO.puts "Rejecting connection. No username."
              {:error, :bad_user}
            end
          end
        end

For more information visit the docs on `Skyline.Auth.AuthHandler`

## Creating a Topic Router

The routing mechanism of Skyline is a trimmed down version of Phoenix's Router/Plug modules. Plug was renamed to Pipe to avoid confusion.

```
defmodule Example.Router do
  use Skyline.Topic.Router
  
  pipeline :userpipeline do
    pipe Example.Pipe.UserGuard, []
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
```

I hope that future releases will make the routing more tuned to the needs of MQTT specifically.
For more information visit the docs on `Skyline.Topic.Router`

## Creating a Controller

The Controller implmentation is also a trimmed down version of Pheonix's controller.

```
defmodule Example.Controller.Admin do
  use Skyline.Topic.Controller
  alias Skyline.Topic.Conn
  alias Example.User
  
  pipe :admin_only, []
  
  def init(opts) do
    opts
  end
  
  def subscribe(conn, _opts) do
    # Do something meaningful like log access or recompute statistics
    conn
  end
  
  def publish(_conn, _opts) do
    {:close_connection, "Admin Topics are read only."}
  end
  
  def admin_only(%Conn{topic: topic,
                       auth_info: %User{username: username,
                                        is_admin: is_admin}} = conn, opts) do
    if username == "admin" do
      IO.puts "Admin topic #{topic} accessed."
      conn
    else
      {:close_connection, "Non-admin #{username} tried to access admin topic."}
    end
  end
end
```

#### Events

You can subscribe to incoming and outgoing messages, errors, logins and disconnects through the event system. This can be handy if you want to build a mailbox topic of all the errors a user has hit. Or simply expand functionality of topics based on other events.

```
defmodule Example.EventHandler do
  use GenEvent
  require Logger
  alias Skyline.Events.Errors
  def init() do
      Errors.add_handler(Example.EventHandler, nil)
  end
  def handle_event({:error, {client_id, auth_info, exception}}, st) do
    Logger.error "Recieved Exception: (#{client_id}, #{inspect auth_info}): #{inspect exception}"
    {:ok, st}
  end
end
```

For more information visit the docs on `Skyline.Events.*`

## Status

This package is pre 0.1, as such the api is subject to change between releases. There are a few missing features that are currently being worked out.

#### MQTT 3.1 features missing
* Last Will
* Persistent Sessions
* Needs to disconnect on errors more often.

#### Nice to haves
* Websockets
* More resiliant Qos handling
* Tests