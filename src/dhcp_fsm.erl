%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 20 May 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(dhcp_fsm).

-behaviour(gen_fsm).

-include("dhcp.hrl").

%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-export([
         initial/2,
         offered/2,
         bound/2
        ]).

-define(SERVER, ?MODULE).

-define(S(S), (1000*S)).

-record(state, {xid=0,
                last={0,0,0},
                handler,
                handler_state,
                socket,
                server_identifier,
                yiaddr}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Socket, Handler) ->
    gen_fsm:start_link(?MODULE, [Socket, Handler], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Socket, Handler]) ->
    {ok, HandlerState, ServerIdentifier} = Handler:init(),
    {ok, initial, #state{handler = Handler,
                         handler_state = HandlerState,
                         socket = Socket,
                         server_identifier = ServerIdentifier,
                         last = erlang:now()}, ?S(1)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
initial(timeout, State) ->
    {stop, normal, State};

initial(Pkg = #dhcp_package{xid = XId, message_type = discover}, State) ->
    case delegate(discover, Pkg, State) of
        {ok, RPkg, State1} ->
            YiAddr = dhcp_package:get_yiaddr(RPkg),
            {next_state, offered, State1#state{xid = XId, yiaddr = YiAddr}, ?S(10)};
        E ->
            lager:warning("[DHCP] callback module returned ~p", [E]),
            {next_state, initial, State#state{xid = XId}, ?S(10)}
    end;

initial(Pkg = #dhcp_package{message_type = request}, State) ->
    case delegate(request, Pkg,
                  [{renewal_time, 3000},
                   {rebinding_time, 3000}], State) of
        {ok, RPkg, State1} ->
            Timeout = dhcp_package:get_option(ip_address_lease_time, RPkg),
            YiAddr = dhcp_package:get_yiaddr(RPkg),
            {next_state, bound, State1#state{yiaddr = YiAddr}, ?S(Timeout)};
        E ->
            lager:warning("[DHCP] callback module returned ~p", [E]),

            {next_state, initial, State, ?S(10)}
    end.

offered(timeout, State) ->
    {stop, normal, State};

offered(Pkg = #dhcp_package{xid = _XId, message_type = request},
        State = #state{xid = _XId, server_identifier = Si, yiaddr = YiAddr}) ->
    case {dhcp_package:get_option(dhcp_server_identifier, Pkg),
          dhcp_package:get_option(requested_ip_address, Pkg)} of
        {Si, YiAddr} ->
            case delegate(request, Pkg, State) of
                {ok, RPkg, State1} ->
                    Timeout = dhcp_package:get_option(ip_address_lease_time, RPkg),
                    {next_state, bound, State#state{last=erlang:now(),
                                                    handler_state = State1}, ?S(Timeout)};
                {ok, State1} ->
                    {next_state, offered, State#state{last=erlang:now(),
                                                      handler_state = State1}, ?S(10)};
                _ ->
                    {stop, normal, State}
            end;
        {Si, _} ->
            lager:error("[DHCP] invalid ip requested!"),
            {stop, normal, State};
        _ ->
            {stop, normal, State}
    end;

offered(#dhcp_package{xid = _XId, message_type = decline}, State = #state{xid = _XId}) ->
    {stop, normal, State}.

bound(timeout, State) ->
    {stop, normal, State};

bound(Pkg = #dhcp_package{xid = _XId, message_type = release},
      State = #state{xid = _XId, handler = M, handler_state = S}) ->
    case M:release(Pkg, S) of
        {ok, S1} ->
            {stop, normal, State#state{handler_state = S1}};
        _ ->
            {stop, normal, State}
    end;
bound(Pkg = #dhcp_package{xid = _XId, message_type = request},
      State = #state{xid = _XId, server_identifier = Si, yiaddr = YiAddr}) ->
    case {dhcp_package:get_option(dhcp_server_identifier, Pkg),
          dhcp_package:get_option(requested_ip_address, Pkg)} of
        {Si, YiAddr} ->
            case delegate(request, Pkg, State) of
                {ok, RPkg, State1} ->
                    Timeout = dhcp_package:get_option(ip_address_lease_time, RPkg),
                    {next_state, bound, State#state{last=erlang:now(),
                                                    handler_state = State1}, Timeout};
                _ ->
                    {stop, normal, State}
            end;
        {Si, _} ->
            lager:error("[DHCP] invalid ip requested!"),
            {stop, normal, State};
        _ ->
            {stop, normal, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
delegate(F, Pkg, State) ->
    delegate(F, Pkg, [], State).

delegate(F, Pkg, Opts, State = #state{handler = M}) ->
    RPkg = lists:foldl(fun({K, V}, P) ->
                               dhcp_package:set_option(K, V, P)
                       end, dhcp_package:clone(Pkg), Opts),
    RPkg1 = dhcp_package:set_op(reply, RPkg),
    case  M:F(RPkg1, Pkg, State#state.handler_state) of
        {ok, Reply, S1} ->
            case dhcp_package:valid_reply(Reply) of
                true ->
                    Dst = reply_addr(Reply),
                    {ok, Bin} = dhcp_package:encode(Reply),
                    gen_udp:send(State#state.socket, Dst, 68, Bin),
                    {ok, Reply, State#state{handler_state = S1, last=erlang:now()}};
                false ->
                    lager:error("[DHCP] invalid reply package for ~p:~p -> ~p", [M, F, Reply])
            end;
        {ok, S1} ->
            {ok, State#state{handler_state = S1, last=erlang:now()}};
        E ->
            E
    end.

reply_addr(#dhcp_package{flags = [broadcast]}) ->
    {255,255,255,255};
reply_addr(#dhcp_package{message_type = offer}) ->
    {255,255,255,255};
reply_addr(#dhcp_package{message_type = nack}) ->
    {255,255,255,255};
reply_addr(#dhcp_package{ciaddr = 0}) ->
    {255,255,255,255};
reply_addr(#dhcp_package{ciaddr = Addr}) ->
    <<A:8, B:8, C:8, D:8>> = <<Addr:32>>,
    {A, B, C, D}.
