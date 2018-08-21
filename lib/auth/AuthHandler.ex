defmodule Skyline.Auth.AuthHandler do
  @moduledoc """
  Defines the methods used by Skyline to connect and authorize a new client.
  """
  @type connection_response :: {:ok, term} |
                               {:error, Skyline.conn_ack_type}
  @callback use(Behaviour) :: Behaviour

  @doc """
  Initializes AuthHandler options. Gets passed to `new_connection/2`.
  """
  @callback init() :: term

  @doc """
  Processes a connect message and either returns `{:ok, auth_info}` or `{:error, conn_ack_type}` where
  auth_info is what is passed to `Skyline.Conn` whenever a message is recieved.

    The second parameter are the opts from `init/0`.
  """
  @callback new_connection(Skyline.Msg.Connect.t, term) :: connection_response

end
