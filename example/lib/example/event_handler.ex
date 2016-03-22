defmodule Example.EventHandler do
  use GenEvent
  
  require Logger
  
  alias Skyline.Events.{Errors, Incoming, Outgoing, Auth}
  alias Example.EventHandler

  def init() do

      # :error events
      Errors.add_handler(EventHandler, nil)
      # :incoming_message events
      Incoming.add_handler(EventHandler, nil)
      # :outgoing_message events
      Outgoing.add_handler(EventHandler, nil)
      # :connect, :disconnect events
      Auth.add_handler(EventHandler, nil)
      
  end

  # Callbacks

  def handle_event({:error, {client_id, auth_info, exception}}, st) do
    Logger.error "Recieved Exception: (#{client_id}, #{inspect auth_info}): #{inspect exception}"
    {:ok, st}
  end

  def handle_event({:incoming_message, {client_id, auth_info, message}}, st) do
    Logger.debug "Accepted message (#{client_id}, #{inspect auth_info}): #{inspect message}"
    {:ok, st}
  end

  def handle_event({:outgoing_message, {client_id, auth_info, message}}, st) do
    Logger.debug "Sent message (#{client_id}, #{inspect auth_info}): #{inspect message}"
    {:ok, st}
  end


  def handle_event({:connect, {client_id, auth_info}}, st) do
    Logger.debug "New Connection: #{client_id} #{inspect auth_info}"
    {:ok, st}
  end

  def handle_event({:disconnect, {client_id, auth_info}}, st) do
    IO.puts "Disconnected: #{client_id} #{inspect auth_info}"
    {:ok, st}
  end

end