defmodule Example.Controller.Admin do
  use Skyline.Topic.Controller
  alias Skyline.Topic.Conn
  alias Skyline.Client
  
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