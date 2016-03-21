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
      {:close_connection, "You can only access your own topics."}
    end
  end
end
