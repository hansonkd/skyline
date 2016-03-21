defmodule Skyline.Events do
  @moduledoc false
  
  use GenEvent

  def error(client_id, auth_info, reason) do
    GenEvent.notify(:skyline_errors, {:error, {client_id, auth_info, reason}})
  end

  def write_message(client_id, auth_info, message) do
    GenEvent.notify(:skyline_outgoing, {:outgoing_message, {client_id, auth_info, message}})
  end

  def accept_message(client_id, auth_info, message) do
    GenEvent.notify(:skyline_incoming, {:incoming_message, {client_id, auth_info, message}})
  end

  def connect(client_id, auth_info) do
    GenEvent.notify(:skyline_auth_events, {:connect, {client_id, auth_info}})
  end

  def disconnect(client_id, auth_info) do
    GenEvent.notify(:skyline_auth_events, {:disconnect, {client_id, auth_info}})
  end

end
