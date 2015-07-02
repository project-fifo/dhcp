-module(dhcp_eqc).

-define(M, dhcp).

-ifdef(TEST).
-ifdef(EQC).
-include_lib("fqc/include/fqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-include("dhcp.hrl").

byte() ->
    choose(0,255).

ip_tpl() ->
    {byte(), byte(), byte(), byte()}.

prop_ip_tpl_conversion() ->
    ?FORALL(Tpl, ip_tpl(),
            begin
                EncDecTpl = ?M:ip_to_tpl(?M:tpl_to_ip(Tpl)),
                EncDecTpl =:= Tpl
            end).

-endif.
-endif.
