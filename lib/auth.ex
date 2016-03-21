defmodule Skyline.Auth do
  @moduledoc false

  alias Skyline.Msg.{Connect, ConnAck}
  alias Skyline.Session

  @type connect_response :: {:ok, ConnAck.t, pid, any} | {:error, ConnAck.t}
  # Reconnects an existing server with a new connection
  defp reconnect(server, connection, client_proc) do
    :gen_server.call(server, {:reconnect, connection, client_proc})
  end

  @spec connect(Skyline.socket, Skyline.Msg.Connect.t, Skyline.Client.t) :: connect_response
  def connect(client, %Connect{client_id: client_id} = con_msg, %Skyline.Client{app_config: config}) do
    case Skyline.Auth.Protocol.new_connection(config.auth, con_msg) do
      {:ok, auth_info} ->
          case Session.start_link(client, client_id, auth_info) do
             {:error, {:already_started, pid}} ->
                {:ok, ConnAck.new(:ok), pid, auth_info}
             {:ok, pid} ->
                {:ok, ConnAck.new(:ok), pid, auth_info}
           end
      {:error, reason} -> {:error, ConnAck.new(reason)}
    end
  end
  
   @doc false
   defmacro __using__(_) do
    quote do
      Module.register_attribute __MODULE__, :skyline_routes, accumulate: true
      
      @behavior Skyline.Topic.Pipe
      
      import Skyline.Topic.Router
      import Skyline.Topic.Router.Route

      # Set up initial scope
      @skyline_pipeline nil
      Skyline.Topic.Scope.init(__MODULE__)
      @before_compile unquote(__MODULE__)
    end
   end
  
  defmacro __before_compile__(env) do
    routes = env.module
    call =
      quote do
        unquote(conn) =
          update_in unquote(conn).private,
            &(&1 |> Map.put(:skyline_pipelines, [])
                 |> Map.put(:skyline_router, __MODULE__))
        unquote(pipeline)
      end

    # line: -1 is used here to avoid warnings if forwarding to root path
    no_match =
      quote line: -1 do
        defp match_route(conn, _action, _path_info) do
          raise NoRouteError, conn: conn, router: __MODULE__
        end
      end

    quote do
      defp do_call(unquote(conn), opts) do
        unquote(call)
      end

      unquote(matches)
      unquote(no_match)
    end
  end
end
