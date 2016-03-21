defmodule MessageHandler do
  use GenEvent

  # Callbacks

  def handle_event({:incoming_message, {client_id, auth_info, message}}, st) do
    IO.puts "Accepted message (#{client_id}, #{inspect auth_info}): #{inspect message}"
    {:ok, st}
  end

  def handle_event({:outgoing_message, {client_id, auth_info, message}}, st) do
    IO.puts  "Sent message (#{client_id}, #{inspect auth_info}): #{inspect message}"
    {:ok, st}
  end

  def handle_event({:error, {client_id, auth_info, exception}}, st) do
    IO.puts "Recieved Exception: (#{client_id}, #{inspect auth_info}): #{inspect exception}"
    {:ok, st}
  end

  def handle_event({:connect, {client_id, auth_info}}, st) do
    IO.puts "New Connection: #{client_id} #{inspect auth_info}"
    {:ok, st}
  end

  def handle_event({:disconnect, {client_id, auth_info}}, st) do
    IO.puts "Disconnected: #{client_id} #{inspect auth_info}"
    {:ok, st}
  end

end

defmodule Example do
  import Skyline.AppConfig

  def init() do
    Example.EventHandler.init
    
    %Skyline.AppConfig{
      auth: %AuthConfig{admins: ["admin"]},
      router: SkylineApp.Router
    }
  end
end
