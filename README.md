This is an attempt to write a proper, liberally licensed DHCP server library in Erlang.

It is written as a extendable library, allowing to delegate the relevant calls on discover / request and release without burdening the implementation with the bookkeeping details required by DHCP.


Running it is pretty simple:

* Start the `dhcp` app.
* Implement a handler.
* Register the handler via a call to `dhcp_server:register_handler/2`

## The handler
The handler is a module implementing the `dhcp_handler` behaviour it has to export the functions, a handler is created for every release request (identified by xid and chaddr):

### init/0
The initialising function it takes no parameters but has to return a tupel of the form:
`{ok, state(), server_id()}` where the `server_id()` is the IP address used to identify the server.

### discover/3
Discover is called on a `discover` request, it takes three parameters:
* ReplyPkg - a minimal reply package with the values from the request already set.
* RequestPkg - the original request package.
* State - The state of the handler.

The function has to return either:
* `{ok, ReplyPkg}` where ReplyPkg was filled 'manually'.
* `{ok, {offer, IP, Netmask[, GWs], ReplyPkg}, State}` - where IP and Netmask are IP addresses to offer, GWs can either be omitted, be a single IP or a list of IP's and ReplyPkg is either the original package or has modification as additional options set on it.
* `{ok, State}` - if no offer is send in reply.
* `{error, Reason}` - an error that shuts down the related FSM.

### request/3
A request in reply to the offer, it is already checked if the requested IP is equal to the offered IP, the parameters passed are in accordance with the discover function:
* ReplyPkg - a minimal reply package with the values from the request already set.
* RequestPkg - the original request package.
* State - The state of the handler.

The funcion has to return:
* `{ok, ReplyPkg}` where ReplyPkg was filled 'manually'.
* `{ok, {ack, [IP, Netmask[, GWs]], ReplyPkg}, State}` - where IP and Netmask are IP addresses to offer, GWs can either be omitted, be a single IP or a list of IP's and ReplyPkg is either the original package or has modification as additional options set on it, all fiends but The ReplyPkg are optional.
* `{ok, {nck,  ReplyPkg}, State}` - The request is denied, details can be set in the ReplyPkg.
* `{error, Reason}` - an error that shuts down the related FSM.

### release/2
Only for informative reasons, the function does not have to return a package, it is passed it's state and the release package and can return either:
* `{ok, State}` - return normal.
* `{error, Reason}` - an error that shuts down the related FSM.

## Configuration
There are three configuration options that can adjust the behavior of the FSM, namely the timeouts.
* initial_timeout - the timeout in seconds until the FSM quits when no initial discover or request is received.
* offer_timeout - the timeout in seconds before a offer has to be accepted or declined.
