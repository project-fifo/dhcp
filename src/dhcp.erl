-module(dhcp).

-include("dhcp.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([ip_to_tpl/1, tpl_to_ip/1]).

ip_to_tpl(I) when is_integer(I) ->
    <<A:8, B:8, C:8, D:8>> = <<I:32>>,
    {A, B, C, D}.

tpl_to_ip({A, B, C, D}) ->
    <<I:32>> = <<A:8, B:8, C:8, D:8>>,
    I.

-ifdef(TEST).
ip2tpl_test() ->
    ?assertEqual({1,2,3,4}, ip_to_tpl(16#01020304)),
    ?assertEqual({0,0,0,0}, ip_to_tpl(16#00000000)),
    ?assertEqual({255,255,255,255}, ip_to_tpl(16#FFFFFFFF)).

tpl2ip_test() ->
    ?assertEqual(16#01020304, tpl_to_ip({1,2,3,4})),
    ?assertEqual(16#00000000, tpl_to_ip({0,0,0,0})),
    ?assertEqual(16#FFFFFFFF, tpl_to_ip({255,255,255,255})).
-endif.

