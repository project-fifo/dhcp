-module(dhcp_package_eqc).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("dhcp/include/dhcp.hrl").

-define(M, dhcp_package).


%%%===================================================================
%%% Generators
%%%===================================================================

byte() ->
    choose(0,255).

null_terminated_string() ->
    ?LET(S,
         list(choose(1,255)),
         list_to_binary(S)).

mac() ->
    {byte(),byte(),byte(),byte(),byte(),byte()}.

op() ->
    oneof([request, reply]).

ip() ->
    choose(0, 4294967295).

message_type() ->
    oneof([discover, offer, request, decline,
           ack, nck, release, inform, force_renew]).

htype() ->
    oneof([ethernet,
           experiemental_ethernet,
           ax25,
           proteon_token_ring,
           chaos,
           ieee802,
           arcnet,
           hyperchannel,
           lanstar,
           autonet_short_address,
           localtalk,
           localnet,
           ultra_link,
           smds,
           frame_relay,
           atm16,
           hdlc,
           fibre_channel,
           atm19,
           serial_line,
           atm21,
           mil_std_188_220,
           metricom,
           ieee1394,
           mapos,
           twinaxial,
           eui64,
           hiparp,
           ip_over_iso_7816_3,
           arpsec,
           ipsec_tunnel,
           infiniband,
           cai_tia_102]).

%%%===================================================================
%%% Properties
%%%===================================================================


prop_mac_conversion() ->
    ?FORALL(Mac, mac(),
            begin
                EncDecMac = ?M:decode_mac(?M:encode_mac(Mac)),
                EncDecMac =:= Mac
            end).

prop_op_conversion() ->
    ?FORALL(Op, op(),
            begin
                EncDecOp = ?M:decode_op(?M:encode_op(Op)),
                EncDecOp =:= Op
            end).

prop_ip_conversion() ->
    ?FORALL(IP, ip(),
            begin
                EncDecIP = ?M:decode_ip(?M:encode_ip(IP)),
                EncDecIP =:= IP
            end).

prop_mt_conversion() ->
    ?FORALL(MT, message_type(),
            begin
                EncDecMT = ?M:decode_message_type(?M:encode_message_type(MT)),
                EncDecMT =:= MT
            end).

prop_htype_conversion() ->
    ?FORALL(HType, htype(),
            begin
                EncDecHType = ?M:decode_htype(?M:encode_htype(HType)),
                EncDecHType =:= HType
            end).

prop_string_conversion() ->
    ?FORALL(S, null_terminated_string(),
            begin
                EncDecS = ?M:decode_string(?M:encode_string(S, byte_size(S) + 2)),
                EncDecS =:= S
            end).

%% prop_package_conversion() ->
%%     ?FORALL(P, dhcp:package(),
%%             begin
%%                 PMt = set_option({message_type, P#dhcp_package.message_type}, P),
%%                 PMt1 = PMt#dhcp_package{
%%                          %% We need make sure options is sorted for compairison
%%                          options = lists:sort(PMt#dhcp_package.options),
%%                          %% We need to work around the fact that there is no notation for
%%                          %% lists with a exact number of elements
%%                          flags = ordsets:from_list(PMt#dhcp_package.flags),
%%                          %% There is no way to notate binarys that do not contain 0's
%%                          %% So we eliminate them and test the conversion sepperately
%%                          sname = <<>>,
%%                          file = <<>>},
%%                 {ok, EncP} = encode(PMt1),
%%                 {ok, EncDecP} = decode(EncP),
%%                 EncDecP =:= PMt1
%%             end).
