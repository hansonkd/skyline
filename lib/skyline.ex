defmodule Skyline do

  use Application

  import Supervisor.Spec, warn: false

  alias Skyline.Msg

  @type socket :: any

  @type keep_alive :: pos_integer | :infinity
  @type qos_type :: :fire_and_forget | :at_least_once | :exactly_once

  @type empty_msg :: :ping_req | :ping_resp | :disconnect
  @type msg_with_id :: :pub_ack | :pub_rec | :pub_comp | :unsub_ack
  @type simple_message_type :: empty_msg | msg_with_id | :conn_ack |
            :pub_rel | :reserved

  @type message_type :: simple_message_type | :connect | :publish |
           :subscribe | :sub_ack | :unsubscribe

  @type conn_ack_type :: :ok | :unaccaptable_protocol_version |
          :identifier_rejected | :server_unavailable | :bad_user |
          :not_authorized

  @type skyline_msg :: Msg.PublishReq.t | Msg.PingReq.t | Msg.PingResp.t |
      Msg.Disconnect.t | Msg.PubAck.t | Msg.PubRec.t | Msg.Unsubscribe.t |
      Msg.Subscribe.t | Msg.SubAck.t | Msg.Connect.t |  Msg.ConnAck.t


  def start(_type, _args) do

    :ets.new(:session_msg_ids, [:named_table, :public])
    :ets.new(:msg_queues, [:named_table, :public])
    
    {:ok, _pid} = Skyline.Events.Errors.start_link()
    {:ok, _pid} = Skyline.Events.Outgoing.start_link()
    {:ok, _pid} = Skyline.Events.Incoming.start_link()
    {:ok, _pid} = Skyline.Events.Auth.start_link()

    port = Application.fetch_env!(:skyline, :port)
    
    auth_mod = Application.fetch_env!(:skyline, :auth_module)
    router_mod = Application.fetch_env!(:skyline, :router_module)
    
    app_config = %Skyline.AppConfig{
        auth_module: auth_mod,
        auth_opts: auth_mod.init,
        router_module: router_mod,
        router_opts: router_mod.init,
    }
    children = [
      worker(Skyline.Acceptor, [app_config, port])
    ]

    opts = [strategy: :one_for_one, name: Skyline.Supervisor]
    Supervisor.start_link(children, opts)

  end

end
