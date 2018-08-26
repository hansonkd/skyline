defmodule Skyline.Session do
  @moduledoc false

  # starting up new subcriptions,
  # and managing the session.

  use Skyline.Amnesia.Session.SessionDatabase
  require Amnesia.Helper

  alias Skyline.Client

  def start_session(%Client{client_id: client_id, persistent_session: persist} = client) do
    Amnesia.transaction do
        fresh_session = %StoredSession{client_id: client_id, topics: []}
        case StoredSession.read(client_id) do
          nil ->
              if persist do
                fresh_session |> StoredSession.write
              end
              {false, client}
          %StoredSession{topics: topics} = session ->
            if persist do
              sub_msg = Skyline.Msg.Subscribe.new(topics, 0)
              case Skyline.Handler.handle_subscribe(topics, sub_msg, [], client) do
                {:close_connection, reason} ->
                    StoredSession.delete(session)
                    fresh_session |> StoredSession.write
                    {false, client}
                {_qos, %Client{} = after_sub} ->
                    {true, after_sub}
              end
            else
                StoredSession.delete(session)
                {false, client}
            end
        end
    end
  end

  def handle_last_will(%Client{client_id: client_id, persistent_session: persist}) do

  end

  def clear_last_will() do
    
  end
end
