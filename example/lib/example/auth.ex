defmodule Example.User do
  @moduledoc "Info about authenticated user passed in Skyline.Conn"
  defstruct username: nil, is_admin: false
end
defmodule Example.DumbAuth do
  defstruct admins: []
  
  @behaviour Skyline.Auth.AuthHandler
  alias Skyline.Msg.Connect
  
  def init() do
      %Example.DumbAuth{admins: ["admin"]}
  end
  def new_connection(%Connect{user_name: username}, %Example.DumbAuth{admins: admins}) do
    if username && String.length(username) > 0 do
      {:ok, %Example.User{username: username, is_admin: username in admins}}
    else
      IO.puts "Rejecting connection. No username."
      {:error, :bad_user}
    end
  end
end