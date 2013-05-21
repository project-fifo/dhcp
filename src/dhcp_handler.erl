-module(dhcp_handler).

-type state() :: any().
-type server_id() :: integer().

-callback init()
	-> {ok, state(), server_id()}.

-callback discover(ReplyPkg, RequestPkg, state()) ->
    {ok, state()} |
    {ok, ReplyPkg, state()} |
    {error, any()} when ReplyPkg::dhcp_package:package(),
                        RequestPkg::dhcp_package:package().

-callback request(ReplyPkg, RequestPkg, state()) ->
    {ok, state()} |
    {ok, ReplyPkg, state()} |
    {error, any()} when ReplyPkg::dhcp_package:package(),
                        RequestPkg::dhcp_package:package().

-callback release(RequestPkg, state()) ->
    {ok, state()} |
    {error, any()} when RequestPkg::dhcp_package:package().


