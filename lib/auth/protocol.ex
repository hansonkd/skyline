defprotocol Skyline.Auth.Protocol do
    @type connection_response :: {:ok, any} |
                                 {:error, Skyline.conn_ack_type}

    @spec new_connection(__MODULE__, Skyline.Msg.Connect.t) :: connection_response
    def new_connection(mod, con_msg)
end
