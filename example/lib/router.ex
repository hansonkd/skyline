"""
Phoenix.ActionClauseError
"""

defmodule MyAuth do
  @moduledoc "Info about authenticated user passed in Skyline.Conn"
  defstruct username: nil, is_admin: false
end
defmodule AuthConfig do
  @moduledoc "Configuration passed to Skyline AppConfig"
  defstruct admins: []
end
defimpl Skyline.Auth.Protocol, for: AuthConfig do
  alias Skyline.Msg.Connect
  def new_connection(%AuthConfig{admins: admins}, %Connect{user_name: username}) do
    if username && String.length(username) > 0 do
      {:ok, %MyAuth{username: username, is_admin: username in admins}}
    else
      IO.puts "Rejecting connection. No username."
      {:error, :bad_user}
    end
  end
end

defmodule Admin do
  use Skyline.Topic.Controller

  alias Skyline.Topic.Conn
  alias Skyline.Client

  pipe :admin_only, []

  def admin_only(%Conn{topic: topic,
                       client: %Client{auth_info: %MyAuth{username: username,
                                                          is_admin: is_admin}}} = conn, opts) do
    if username == "admin" do
      IO.puts "Admin topic #{topic} accessed."
      conn
    else
      {:close_connection, "Non-admin #{username} tried to access admin topic."}
    end
  end

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
end

defmodule LocationPublish do
  use Skyline.Topic.Controller

  alias Skyline.Topic.Conn
  alias Skyline.Msg.PublishReq

  def init(opts) do
    opts
  end

  def publish(%Conn{message: %PublishReq{message: body},
                    params: %{"username" => username}} = conn, _opts) do
    IO.puts "User #{username} updated their location to #{body}"
    conn
  end

  def subscribe(%Conn{topic: topic, params: %{"username" => username}}=conn, _opts) do
    IO.puts "New subscription to #{username}'s topic #{topic}."
    conn
  end

end

defmodule UserNoMatchController do
  use Skyline.Topic.Controller

  def init(opts) do
    opts
  end

  def publish(_conn, _opts) do
    {:close_connection, "Not a prechosen topic."}
  end

  def subscribe(_conn, _opts) do
    {:close_connection, "Not a prechosen topic."}
  end

end

defmodule UserGuard do
  alias Skyline.Topic.Conn
  alias Skyline.Client

  def init(opts) do
    opts
  end

  def call(%Conn{topic: topic,
                client: %Client{auth_info: %MyAuth{username: auth_user, is_admin: is_admin}},
                params: %{"username" => username}} = conn, opts) do

    if (auth_user == username) or is_admin do
      IO.puts "User #{auth_user} is accessing #{username}"
      conn
    else
      {:close_connection, "You can only access your topics."}
    end
  end
end

defmodule SkylineApp.Router do
  use Skyline.Topic.Router

  pipeline :userpipeline do
    pipe UserGuard, []
  end

  scope "" do
    resource("$*admin", Admin)

    scope "user/:username/" do
      pipe_through :userpipeline

      resource("location", LocationPublish)

      # You can specify the publish and subscribe controllers seperately.
      # The can be two different modules or in one.
      publish("*other", UserNoMatchController)
      subscribe("*other", UserNoMatchController)
    end
  end
  # This will catch everything else. The default resource controller will
  # allow all publish and subscribes.
  scope "", alias: Skyline.Topic.Controller.Default do
    resource("*path", ResourceController)
  end
end
