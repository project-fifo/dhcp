-module(dhcp_eqc).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("dhcp/include/dhcp.hrl").

-define(M, dhcp).

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
