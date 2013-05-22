%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 20 May 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(dhcp_server).

-behaviour(gen_server).

-include("dhcp.hrl").

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([match/2, match_pkg/2,
         match_mac/2, match_fields/2, match_opts/2]).
-endif.

%% API
-export([start_link/0, register_handler/2]).
-ignore_xref([start_link/0, register_handler/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TBL, dhcp_sessions).

-record(state, {socket, handlers = []}).
-type mac_match_byte() :: byte() | '_'.
-type mac_match() :: '_' | {mac_match_byte(), mac_match_byte(), mac_match_byte(), mac_match_byte(), mac_match_byte(), mac_match_byte()}.
-type field_match() :: {dhcp:package_fields(),
                        dhcp:op() | dhcp:htype() | byte() | dhcp:int32() |
                        dhcp:ip() | dhcp:mac() | binary() | dhcp:message_type() |
                        dhcp:flags()}.
-type option_match() :: dhcp:option().
-type match_spec() :: {mac_match(), [field_match()], [option_match()]}.



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
-spec register_handler(Handler::atom(), Spec::match_spec()) -> ok | {error, _}.
register_handler(Handler, Spec = {_,_,_}) when is_atom(Handler)->
    gen_server:call(?SERVER, {register, Handler, Spec}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Socket} = gen_udp:open(67, [binary,
                                     inet,
                                     {broadcast, true},
                                     {reuseaddr, true}]),
    ets:new(?TBL, [bag, {read_concurrency, true}, named_table]),

    {ok, #state{socket = Socket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({register, H, Spec}, _From, State = #state{handlers = Hs}) ->
    Reply = ok,
    {reply, Reply, State#state{ handlers = [{H, Spec} | Hs]}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info ({udp, Socket, _IP, 68, Packet}, State = #state{socket=Socket, handlers = Handler}) ->
    case dhcp_package:decode(Packet) of
        {ok, D} ->
            MT = D#dhcp_package.message_type,
            ID  = {D#dhcp_package.chaddr, D#dhcp_package.xid},
            case {ets:lookup(?TBL, ID), MT} of
                {[], discover} ->
                    case match(D, Handler) of
                        {ok, H} ->
                            {ok, Pid} = supervisor:start_child(dhcp_fsm_sup, [Socket, H]),
                            gen_fsm:send_event(Pid, D),
                            ets:insert(?TBL, {ID, Pid});
                        _ ->
                            ok
                    end;
                {[{ID, Pid}], _} ->
                    case  process_info(Pid) of
                        undefined ->
                            case match(D, Handler) of
                                {ok, H} ->
                                    {ok, Pid1} = supervisor:start_child(dhcp_fsm_sup, [Socket, H]),
                                    gen_fsm:send_event(Pid1, D),
                                    ets:insert(?TBL, {ID, Pid1});
                                _ ->
                                    ok
                            end;
                        _ ->
                            gen_fsm:send_event(Pid, D)
                    end;
                _ ->
                    ok
            end;
        E ->
            lager:warning("Decoding failed: ~p (~p)", [E, Packet])
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec match(Pkg::dhcp:package(), [{Handler::atom(), Spec::match_spec()}]) -> {ok, Handler::atom()} | undefined.
match(Pkg, [{Handler, Spec} | R]) ->
    case match_pkg(Pkg, Spec) of
        true ->
            {ok, Handler};
        false ->
            match(Pkg, R)
    end;
match(_Pkg, []) ->
    undefined.

-spec match_pkg(Pkg::dhcp:package(), Spec::match_spec()) -> boolean().
match_pkg(Pkg, {Mac, Fields, Opts}) ->
    match_mac(dhcp_package:get_chaddr(Pkg), Mac) andalso
        match_fields(Pkg, Fields) andalso
        match_opts(Pkg, Opts).

-spec match_mac(dhcp:mac(), mac_match()) -> boolean().
match_mac({A, B, C, D, E, F}, {A, B, C, D, E, F}) -> true;
match_mac({A, B, C, D, E, _}, {A, B, C, D, E, '_'}) -> true;
match_mac({A, B, C, D, _, _}, {A, B, C, D, '_', '_'}) -> true;
match_mac({A, B, C, _, _, _}, {A, B, C, '_', '_', '_'}) -> true;
match_mac({A, B, _, _, _, _}, {A, B, '_', '_', '_', '_'}) -> true;
match_mac({A, _, _, _, _, _}, {A, '_', '_', '_', '_', '_'}) -> true;
match_mac({_, _, _, _, _, _}, {'_', '_', '_', '_', '_', '_'}) -> true;
match_mac({_, _, _, _, _, _}, '_') -> true;
match_mac(_,_) -> false.

-spec match_fields(P::dhcp:package(), [field_match()]) ->
                          boolean().
match_fields(P, [{F, V} | Fs]) ->
    (dhcp_package:get_field(F, P) =:= V) andalso
        match_fields(P, Fs);
match_fields(_P, []) ->
    true.

-spec match_opts(P::dhcp:package(), [dhcp:option()]) -> boolean().
match_opts(P, [{O, V} | Fs]) ->
    (dhcp_package:get_option(O, P) =:= V) andalso
        match_fields(P, Fs);
match_opts(_, []) ->
    true.

-ifdef(TEST).
propper_test() ->
    ?assertEqual(true, proper:check_spec({dhcp_server, match, 2}, [{to_file, user}, long_result])),
    ?assertEqual(true, proper:check_spec({dhcp_server, match_pkg, 2}, [{to_file, user}, long_result])),
    ?assertEqual(true, proper:check_spec({dhcp_server, match_mac, 2}, [{to_file, user}, long_result])),
    ?assertEqual(true, proper:check_spec({dhcp_server, match_fields, 2}, [{to_file, user}, long_result])),
    ?assertEqual(true, proper:check_spec({dhcp_server, match_opts, 2}, [{to_file, user}, long_result])).

-endif.
