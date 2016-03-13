defmodule SpotApp do
  use Application

  @type qos_type :: :fire_and_forget | :at_least_once | :exactly_once
  @type simple_message_type :: :conn_ack | :pub_ack |
            :pub_rec | :pub_rel | :pub_comp | :unsub_ack |
            :ping_req | :ping_resp | :disconnect |
           :reserved
  @type message_type :: simple_message_type | :connect | :publish |
           :subscribe | :sub_ack | :unsubscribe

  @type conn_ack_type :: :ok | :unaccaptable_protocol_version |
          :identifier_rejected | :server_unavailable | :bad_user |
          :not_authorized


  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    :ets.new(:session_msg_ids, [:set, :named_table, :public])

    children = [
      worker(SpotApp.Worker, [8000])
    ]

    opts = [strategy: :one_for_one, name: SpotApp.Supervisor]
    Supervisor.start_link(children, opts)

  end

end
