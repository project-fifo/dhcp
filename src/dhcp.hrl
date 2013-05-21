-type message_type() ::
        discover | offer | request | decline |
        ack | nack | reelase | inform | force_renew.

-type dhcp_op() :: request | reply.

-type mac() :: {pos_integer(),pos_integer(),pos_integer(),pos_integer(),pos_integer(),pos_integer()}.

-record(dhcp_package,
        {op = request :: dhcp_op(),
         htype :: atom(),
         hlen = 0 :: pos_integer(),
         hops = 0 :: pos_integer(),
         xid = 0 :: pos_integer(),
         secs = 0 :: pos_integer(),
         flags = [] :: [bradcast],
         ciaddr = 0 :: pos_integer(),
         yiaddr = 0 :: pos_integer(),
         siaddr = 0 :: pos_integer(),
         giaddr = 0 :: pos_integer(),
         chaddr = {0,0,0,0,0,0}:: mac(),
         sname = <<>> :: binary(),
         file = <<>> :: binary(),
         options = [] :: [{message_type, message_type()}|{atom(), binary()|integer()|[integer()]}],
         message_type :: message_type()
        }).
