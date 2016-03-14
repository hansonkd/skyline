defmodule Skiline do
  use Application
  import Supervisor.Spec, warn: false

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


  def start(_type, _args) do

    :ets.new(:session_msg_ids, [:set, :named_table, :public])

    children = [
      worker(Skiline.Acceptor, [8000])
    ]

    opts = [strategy: :one_for_one, name: Skiline.Supervisor]
    Supervisor.start_link(children, opts)

  end

end
