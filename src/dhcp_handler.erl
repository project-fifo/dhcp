-module(dhcp_handler).

-type state() :: any().
-type server_id() :: dhcp:ip().

-callback init()
	-> {ok, state(), server_id()}.

-callback discover(ReplyPkg, RequestPkg, state()) ->
    {ok, state()} |
    {ok, {offer, IP, Netmask, ReplyPkg}, state()} |
    {ok, {offer, IP, Netmask, GW, ReplyPkg}, state()} |
    {ok, {offer, IP, Netmask, [GW], ReplyPkg}, state()} |
    {ok, ReplyPkg, state()} |
    {error, any()} when ReplyPkg::dhcp:package(),
                        RequestPkg::dhcp:package(),
                        IP::dhcp:ip(),
                        Netmask::dhcp:ip(),
                        GW::dhcp:ip().


-callback request(ReplyPkg, RequestPkg, state()) ->
    {ok, state()} |
    {ok, {ack, ReplyPkg}, state()} |
    {ok, {ack, IP, Netmask, ReplyPkg}, state()} |
    {ok, {ack, IP, Netmask, GW, ReplyPkg}, state()} |
    {ok, {ack, IP, Netmask, [GW], ReplyPkg}, state()} |
    {ok, {nack, ReplyPkg}, state()} |
    {ok, ReplyPkg, state()} |
    {error, any()} when ReplyPkg::dhcp:package(),
                        RequestPkg::dhcp:package(),
                        IP::dhcp:ip(),
                        Netmask::dhcp:ip(),
                        GW::dhcp:ip().

-callback release(RequestPkg, state()) ->
    {ok, state()} |
    {error, any()} when RequestPkg::dhcp:package().
