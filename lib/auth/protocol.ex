defprotocol Skyline.Auth.Protocol do
    @type connection_response :: {:ok, any} |
                                 {:error, Skyline.conn_ack_type}

    @spec new_connection(Skyline.Msg.Connect.t) :: connection_response
    def new_connection(con_msg)
end
