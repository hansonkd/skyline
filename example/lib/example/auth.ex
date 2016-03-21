defmodule Example.MyAuth do
  @moduledoc "Info about authenticated user passed in Skyline.Conn"
  defstruct username: nil, is_admin: false
end
defmodule Example.AuthConfig do
  @moduledoc "Configuration passed to Skyline AppConfig"
  defstruct admins: []
end
defimpl Skyline.Auth.Protocol, for: Example.AuthConfig do
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
