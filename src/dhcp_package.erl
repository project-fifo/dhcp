-module(dhcp_package).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("dhcp.hrl").

-opaque package() :: #dhcp_package{}.
-export_type([package/0]).

-export([decode/1, encode/1, clone/1]).

-export([valid_reply/1, valid_request/1]).

-export([set_option/3,
         ensure_option/3,
         set_op/2,
         set_htype/2,
         set_hlen/2,
         set_hops/2,
         set_xid/2,
         set_secs/2,
         set_flags/2,
         set_ciaddr/2,
         set_yiaddr/2,
         set_siaddr/2,
         set_giaddr/2,
         set_chaddr/2,
         set_sname/2,
         set_file/2,
         set_options/2,
         set_message_type/2]).

-export([get_option/2,
         get_option/3,
         get_op/1,
         get_htype/1,
         get_hlen/1,
         get_hops/1,
         get_xid/1,
         get_secs/1,
         get_flags/1,
         get_ciaddr/1,
         get_yiaddr/1,
         get_siaddr/1,
         get_giaddr/1,
         get_chaddr/1,
         get_sname/1,
         get_file/1,
         get_options/1,
         get_message_type/1]).

decode(<<Op:8/integer, HType:8/integer, HLen:8/integer, Hops:8/integer,
         XId:32/integer,
         Secs:8/integer, Flags:1/binary,
         CiAddr:32/integer,
         YiAddr:32/integer,
         SiAddr:32/integer,
         GiAddr:32/integer,
         ChAddr:16/binary,
         SName:64/binary,
         File:128/binary,
         Options/binary>>) ->
    DecodedOptions = decode_options(Options),
    case get_option(message_type, DecodedOptions) of
        undefined ->
            {error, message_type};
        MT ->
            {ok, #dhcp_package{
                    op = decode_op(Op),
                    htype = decode_htype(HType),
                    hlen = HLen,
                    hops = Hops,
                    xid = XId,
                    secs = Secs,
                    flags = decode_flags(Flags),
                    ciaddr = decode_ip(CiAddr),
                    yiaddr = decode_ip(YiAddr),
                    siaddr = decode_ip(SiAddr),
                    giaddr = decode_ip(GiAddr),
                    chaddr = decode_mac(ChAddr),
                    sname = decode_string(SName),
                    file = decode_string(File),
                    options = DecodedOptions,
                    message_type = MT
                   }}
    end;

decode(P) ->
    lager:error("[DHCP] Bad package: ~p", [P]),
    {error, bad_package}.

encode(#dhcp_package{
          op = Op, htype = HType, hlen = HLen, hops = Hops,
          xid = XId,
          secs = Secs, flags = Flags,
          ciaddr = CiAddr,
          yiaddr = YiAddr,
          siaddr = SiAddr,
          giaddr = GiAddr,
          chaddr = ChAddr,
          sname = SName,
          file = File,
          options = Options,
          message_type = MT
         }) ->
    Options1 = lists:keystore(message_type, 1, Options, {message_type, MT}),
    {ok, <<(encode_op(Op)):8/integer, (encode_htype(HType)):8/integer, HLen:8/integer, Hops:8/integer,
           XId:32/integer,
           Secs:8/integer, (encode_flags(Flags)):1/binary,
           (encode_ip(CiAddr)):32/integer,
           (encode_ip(YiAddr)):32/integer,
           (encode_ip(SiAddr)):32/integer,
           (encode_ip(GiAddr)):32/integer,
           (encode_mac(ChAddr)):128/integer,
           (encode_string(SName, 64)):64/binary,
           (encode_string(File, 128)):128/binary,
           (encode_options(Options1))/binary>>}.


clone(M) ->
    M#dhcp_package{
      hops = 0,
      flags = [],
      sname = <<>>,
      file = <<>>,
      options = [],
      message_type = undefined
     }.

-define(OP(Id, Atom),
        op(decode, Id) ->
               Atom;
            op(encode, Atom) ->
               Id).
?OP(1, request);
?OP(2, reply).

decode_op(ID) ->
    op(decode, ID).

encode_op(Atom) ->
    op(encode, Atom).

-define(HTYPE(Atom, Id),
        htype(decode, Id) ->
               Atom;
            htype(encode, Atom) ->
               Id).

?HTYPE(ethernet,               1);
?HTYPE(experiemental_ethernet, 2);
?HTYPE(ax25,                   3);
?HTYPE(proteon_token_ring,     4);
?HTYPE(chaos,                  5);
?HTYPE(ieee802,                6);
?HTYPE(arcnet,                 7);
?HTYPE(hyperchannel,           8);
?HTYPE(lanstar,                9);
?HTYPE(autonet_short_address, 10);
?HTYPE(localtalk,             11);
?HTYPE(localnet,              12);
?HTYPE(ultra_link,            13);
?HTYPE(smds,                  14);
?HTYPE(frame_relay,           15);
?HTYPE(atm16,                 16);
?HTYPE(hdlc,                  17);
?HTYPE(fibre_channel,         18);
?HTYPE(atm19,                 19);
?HTYPE(serial_line,           20);
?HTYPE(atm21,                 21);
?HTYPE(mil_std_188_220,       22);
?HTYPE(metricom,              23);
?HTYPE(ieee1394,              24);
?HTYPE(mapos,                 25);
?HTYPE(twinaxial,             26);
?HTYPE(eui64,                 27);
?HTYPE(hiparp,                28);
?HTYPE(ip_over_iso_7816_3,    29);
?HTYPE(arpsec,                30);
?HTYPE(ipsec_tunnel,          31);
?HTYPE(infiniband,            32);
?HTYPE(cai_tia_102,           33).

get_option(Option, Message) ->
    get_option(Option, undefined, Message).

get_option(Option, Default, Message) ->
    case lists:keyfind(Option, 1, Message) of
        {Option, Value} ->
            Value;
        _ ->
            Default
    end.

set_option(Key, Value, Message) ->
    Message#dhcp_package{
      options =
          lists:keystore(Key, 1, Message#dhcp_package.options, {Key, Value})}.

ensure_option(Key, Value, Message) ->
    case get_option(Key, Message) of
        undefined ->
            set_option(Key, Value, Message);
        _ ->
            Message
    end.

set_op(Op, M) ->
    M#dhcp_package{op = Op}.
set_htype(HType, M) ->
    M#dhcp_package{htype = HType}.
set_hlen(HLen, M) ->
    M#dhcp_package{hlen = HLen}.
set_hops(Hops, M) ->
    M#dhcp_package{hops = Hops}.
set_xid(XId, M) ->
    M#dhcp_package{xid = XId}.
set_secs(Secs, M) ->
    M#dhcp_package{secs = Secs}.
set_flags(Flags, M) ->
    M#dhcp_package{flags = Flags}.
set_ciaddr(CiAddr, M) ->
    M#dhcp_package{ciaddr = CiAddr}.
set_yiaddr(YiAddr, M) ->
    M#dhcp_package{yiaddr = YiAddr}.
set_siaddr(SiAddr, M) ->
    M#dhcp_package{siaddr = SiAddr}.
set_giaddr(GiAddr, M) ->
    M#dhcp_package{giaddr = GiAddr}.
set_chaddr(ChAddr, M) ->
    M#dhcp_package{chaddr = ChAddr}.
set_sname(SName, M) ->
    M#dhcp_package{sname = SName}.
set_file(File, M) ->
    M#dhcp_package{file = File}.
set_options(Options, M) ->
    M#dhcp_package{options = Options}.
set_message_type(MT, M) ->
    M#dhcp_package{message_type = MT}.

get_op(M) ->
    M#dhcp_package.op.
get_htype(M) ->
    M#dhcp_package.htype.
get_hlen(M) ->
    M#dhcp_package.hlen.
get_hops(M) ->
    M#dhcp_package.hops.
get_xid(M) ->
    M#dhcp_package.xid.
get_secs(M) ->
    M#dhcp_package.secs.
get_flags(M) ->
    M#dhcp_package.flags.
get_ciaddr(M) ->
    M#dhcp_package.ciaddr.
get_yiaddr(M) ->
    M#dhcp_package.yiaddr.
get_siaddr(M) ->
    M#dhcp_package.siaddr.
get_giaddr(M) ->
    M#dhcp_package.giaddr.
get_chaddr(M) ->
    M#dhcp_package.chaddr.
get_sname(M) ->
    M#dhcp_package.sname.
get_file(M) ->
    M#dhcp_package.file.
get_options(M) ->
    M#dhcp_package.options.
get_message_type(M) ->
    M#dhcp_package.message_type.

decode_htype(T) ->
    htype(decode, T).

encode_htype(T) ->
    htype(encode, T).

decode_flags(<<1:1/integer, _:7>>) ->
    [broadcast];
decode_flags(<<0:1/integer, _:7>>) ->
    [].

encode_flags([]) ->
    <<0:8/integer>>;
encode_flags([broadcast]) ->
    <<1:1/integer, 0:8/integer>>.

decode_ip(IP) ->
    IP.

encode_ip(IP) ->
    IP.

decode_mac(<<A:8, B:8, C:8, D:8, E:8, F:8, _/binary>>) ->
    {A, B, C, D, E, F}.

encode_mac({A, B, C, D, E, F}) ->
    <<A:8, B:8, C:8, D:8, E:8, F:8, 0:80>>.

decode_string(S) ->
    decode_string(S, <<>>).

decode_string(<<0:8, _/binary>>, Bin) ->
    Bin;

decode_string(<<X:8, R/binary>>, Bin) ->
    decode_string(R, <<Bin/binary, X:8>>).

encode_string(B, Max) ->
    case byte_size(B) of
        L when L < Max ->
            Padding = Max - L,
            <<B/binary, 0:(Padding*8)/integer>>
    end.

-define(OPT(Atom, Num, Type),
        opt(decode, Num) ->  {ok, Atom};
            opt(encode, Atom) ->  {ok, Num};
            opt(type, Atom) -> {ok, Type}).

?OPT(pad,                              0,    octets);
?OPT(subnet_mask,                      1,    ipaddr);
?OPT(time_offset,                      2,    integer);
?OPT(router_address,                   3,    ipaddrs);
?OPT(time_server,                      4,    ipaddrs);
?OPT(ien_116_name_server,              5,    ipaddrs);
?OPT(domain_name_server,               6,    ipaddrs);
%% logging_server addresses
?OPT(log_server,                       7,    ipaddrs);
?OPT(quotes_server,                    8,    ipaddrs);
?OPT(lpr_server,                       9,    ipaddrs);
?OPT(impress_server,                  10,    ipaddrs);
?OPT(rlp_server,                      11,    ipaddrs);
%% hostname string
?OPT(hostname,                        12,    string);
%% size of boot file in 512 byte
?OPT(boot_file_size,                  13,    short);
%% client to dump and name
?OPT(merit_dump_file,                 14,    octets);
?OPT(domain_name,                     15,    string);
?OPT(swap_server,                     16,    ipaddr);
%% path name for root disk
?OPT(root_path,                       17,    string);
?OPT(bootp_extensions_path,           18,    string);
?OPT(ip_forward_enable,               19,    byte);
?OPT(source_route_enable,             20,    byte);
%% routing policy filters
?OPT(policy_filter,                   21,    octets);
?OPT(max_datagram_reassembly_sz,      22,    short);
?OPT(default_ip_ttl,                  23,    octets);
?OPT(path_mtu_aging_timeout,          24,    integer);
?OPT(path_mtu_plateau_table,          25,    shorts);
?OPT(interface_mtu_size,              26,    short);
?OPT(all_subnets_are_local,           27,    byte);
?OPT(broadcast_address,               28,    ipaddr);
?OPT(perform_mask_discovery,          29,    byte);
?OPT(provide_mask_to_others,          30,    byte);
?OPT(perform_router_discovery,        31,    byte);
?OPT(router_solicitation_address,     32,    ipaddr);
%% first is destination address, second is router.
?OPT(static_routes,                   33,    ipaddrs);
?OPT(trailer_encapsulation,           34,    byte);
?OPT(arp_cache_timeout,               35,    integer);
?OPT(ethernet_encapsulation,          36,    byte);
?OPT(default_tcp_ttl,                 37,    byte);
?OPT(keep_alive_interval,             38,    integer);
?OPT(keep_alive_garbage,              39,    byte);
?OPT(nis_domain_name,                 40,    string);
?OPT(nis_servers,                     41,    ipaddrs);
?OPT(ntp_servers,                     42,    ipaddrs);
%% vendor specific information
?OPT(vendor,                          43,    octets); %% tlv
?OPT(netbios_name_servers,            44,    ipaddrs);
?OPT(netbios_dgm_dist_servers,        45,    ipaddrs);
?OPT(netbios_node_type,               46,    byte);
%% netbios scope
?OPT(netbios,                         47,    octets);
?OPT(x_window_font_server,            48,    ipaddrs);
?OPT(x_window_display_mgr,            49,    ipaddrs);
?OPT(requested_ip_address,            50,    ipaddr);
?OPT(ip_address_lease_time,           51,    integer);
%% overload "sname" or "file"
?OPT(overload,                        52,    byte);
?OPT(message_type,                    53,    message_type);
?OPT(dhcp_server_identifier,          54,    ipaddr);

%% array of 1_byte numbers indicating which options the client
%% would like to see in the response.
?OPT(parameter_request_list,          55,    bytes);
?OPT(dhcp_error_message,              56,    octets);
?OPT(dhcp_maximum_msg_size,           57,    short);
?OPT(renewal_time,                    58,    integer);
?OPT(rebinding_time,                  59,    integer);
?OPT(vendor_class_identifier,         60,    string);

%% client identifier
%% first octets is dhcp_hardware_type, rest are type_specific data,
%% e.g. mac address.
?OPT(client_identifier,               61,    octets);
?OPT(netware_domain_name,             62,    octets);
?OPT(netware_sub_options,             63,    octets);
?OPT(nis_client_domain_name,          64,    octets);
?OPT(nis_server_address,              65,    ipaddr);
?OPT(tftp_server_name,                66,    string);
?OPT(boot_file_name,                  67,    string);
%% home agent addresses
?OPT(home_agent_address,              68,    octets);
?OPT(smtp_server_address,             69,    ipaddrs);
?OPT(pop3_server_address,             70,    ipaddrs);
?OPT(nntp_server_address,             71,    ipaddrs);
?OPT(www_server_address,              72,    ipaddrs);
?OPT(finger_server_address,           73,    ipaddrs);
?OPT(irc_server_address,              74,    ipaddrs);
?OPT(streettalk_server_address,       75,    ipaddrs);
?OPT(stda_server_address,             76,    ipaddrs);
%% user class information
?OPT(user_class,                      77,    octets);
%% directory agent information
?OPT(directory_agent,                 78,    octets);
%% service location agent scope
?OPT(service_scope,                   79,    octets);
%% rapid commit
?OPT(rapid_commit,                    80,    octets);
%% fully qualified domain name
?OPT(client_fqdn,                     81,    string);
%% relay agent information
?OPT(relay_agent_information,         82,    octets);

%% internet storage name service
?OPT(isns,                            83,    octets);
%% novell directory services
?OPT(nds_servers,                     85,    octets);
%% novell directory services
?OPT(nds_tree_name,                   86,    octets);
%% novell directory services
?OPT(nds_context,                     87,    octets);
%% authentication
?OPT(authentication,                  90,    octets);

?OPT(client_last_txn_time,            91,    octets);

?OPT(associated_ip,                   92,    octets);
%% client system architecture
?OPT(client_system,                   93,    octets);
%% client network device interface
?OPT(client_ndi,                      94,    octets);
%% lightweight directory access protocol
?OPT(ldap,                            95,    octets);
%% uuid/guid_based client identifier
?OPT(uuid_guid,                       97,    octets);
%% open group's user authentication
?OPT(user_auth,                       98,    octets);
%% netinfo parent_server address
?OPT(netinfo_address,                112,    octets);
%% netinfo parent_server tag
?OPT(netinfo_tag,                    113,    octets);
%% url
?OPT(url,                            114,    octets);
%% dhcp auto_configuration
?OPT(auto_config,                    116,    byte);
%% name service search
?OPT(name_service_search,            117,    octets);
%% subnet selection option
?OPT(subnet_selection_option,        118,    octets);
%% dns domain serach list
?OPT(domain_search,                  119,    octets);
%% sip_servers dhcp option
?OPT(sip_servers_dhcp_option,        120,    octets);
%% classless static route option
?OPT(classless_static_route,         121,    octets);
%% cablelabs client configuration
?OPT(ccc,                            122,    octets);
%% 16 geoconf option
?OPT(geoconf_option,                 123,    octets);

%% vendor class
%%
%% string name that defines the vendor space used for the tlv's
%% in option 125.
%%
?OPT(v_i_vendor_class,               124,    octets);
%% vendor_specific
?OPT(v_i_vendor_specific,            125,    octets); % tlv
%% 6 bytes: e4:45:74:68:00:00
?OPT(etherboot,                      128,    octets);

?OPT(call_server_ip_address,         129,    octets);

?OPT(ethernet_interface,             130,    octets);

?OPT(remote_stats_svr_ip_address,    131,    octets);

?OPT(ieee_802_1q_l2_priority,        132,    octets);

?OPT(ieee_802_1p_vlan_id,            133,    octets);

?OPT(diffserv_code_point,            134,    octets);

?OPT(http_proxy,                     135,    octets);

?OPT(cisco_tftp_server_ip_addresses, 150,    ipaddrs);

?OPT(end_of_options,                 255,    byte);

opt(Action, Option) ->
    lager:warning("[DHCP] Unknown option action: ~p -> ~p", [Action, Option]),
    {error, unsupported}.


encode_option(Opt) ->
    opt(encode, Opt).

decode_option(Opt) ->
    opt(decode, Opt).

option_type(Opt) ->
    opt(type, Opt).

decode_type(integer, <<L:8, I:L, R/binary>>) when  L > 1 ->
    {I, R};
decode_type(octets, <<L:8, I:L/binary, R/binary>>) when  L > 1 ->
    {I, R};
decode_type(ipaddr, <<4:8, IP:32, R/binary>>) ->
    {IP, R};
decode_type(ipaddrs, <<L:8, IPs:L/binary, R/binary>>) when L > 4,
                                                           L rem 4 =:= 0->
    {[IP || <<IP:32>> <= IPs], R};
decode_type(byte, <<1:8, B:8, R/binary>>) ->
    {B, R};
decode_type(bytes, <<L:8, Bs:L/binary, R/binary>>) ->
    {[B || <<B:8>> <= Bs], R};
decode_type(short, <<2:8, S:16, R/binary>>) ->
    {S, R};
decode_type(shorts, <<L:8, Ss:L/binary, R/binary>>) when L > 2,
                                                         L rem 2 =:= 0->
    {[S || <<S:16>> <= Ss], R};
decode_type(string, <<L:8, S:L/binary, R/binary>>) when  L > 1 ->
    {S, R};
decode_type(message_type, <<1:8, T:8, R/binary>>) ->
    {decode_message_type(T), R}.

encode_type(octets, B) when is_binary(B) ->
    case byte_size(B) of
        L when L =< 255,
               L >= 1 ->
            <<L:8, B/binary>>
    end;
encode_type(ipaddr, IP) when is_integer(IP) ->
    <<4:8, IP:32>>;
encode_type(ipaddrs, IPs) when is_list(IPs) ->
    case length(IPs) of
        L when (L*4) =< 255,
               L >= 1 ->
            <<(L*4):8, << <<IP:32>> || IP <- IPs, is_integer(IP) >>/binary>>
    end;
encode_type(byte, B) when is_integer(B) ->
    <<1:8, B:8>>;
encode_type(bytes, Bs) when is_list(Bs) ->
    case length(Bs) of
        L when L =< 255,
               L >= 1 ->
            <<L:8, << <<B:8>> || B <- Bs, is_integer(B) >>/binary>>
    end;
encode_type(short, S) when is_integer(S) ->
    <<2:8, S:18>>;
encode_type(shorts, Ss) when is_list(Ss) ->
    case length(Ss) of
        L when (L*2) =< 255,
               L >= 1 ->
            <<(L*2):8, << <<S:16>> || S <- Ss, is_integer(S) >>/binary>>
    end;
encode_type(string, S) when is_binary(S) ->
    case byte_size(S) of
        L when L =< 255,
               L >= 1 ->
            <<L:8, S/binary>>
    end;
encode_type(message_type, Type) ->
    <<1:8, (encode_message_type(Type)):8>>.

decode_options(<<99:8, 130:8, 83:8, 99:8, 0:8, Opts>>) ->
    decode_options(Opts, []).

decode_options(<<255:8, _/binary>>, Opts) ->
    Opts;

decode_options(<<>>, Opts) ->
    Opts;

decode_options(<<Opt:8, R/binary>>, Opts) ->
    case decode_option(Opt) of
        {ok, Name} ->
            {ok, Type} = option_type(Opt),
            {V, R1} = decode_type(Type, R),
            decode_options(R1, [{Name, V} | Opts]);
        _ ->
            lager:warning("[DHCP] Unknown Option: ~p", [Opt]),
            <<L:8, _:L/binary, R1/binary>> = R,
            decode_options(R1, Opts)
    end.

-define(MTYPE(Atom, Id),
        message_type(decode, Id) ->
               Atom;
            message_type(encode, Atom) ->
               Id).

encode_option_pack({K, V}) ->
    case encode_option(K) of
        {ok, ID} ->
            {ok, Type} = option_type(ID),
            <<ID:8, (encode_type(Type, V))/binary>>;
        _ ->
            lager:warning("[DHCP] Trying to encode unsupported option pair ~p: ~p", [K, V]),
            <<>>
    end.

encode_options(Opts) ->
    Content = << <<(encode_option_pack(Opt))/binary>> || Opt <- Opts >>,
    S = byte_size(Content),
    Padding = case (S rem 4) of
                  3 -> <<0:8>>;
                  2 -> <<0:16>>;
                  1 -> <<0:24>>;
                  _ -> <<>>
              end,
    << 99:8, 130:8, 83:8, 99:8, Content/binary, 255:1, Padding/binary >>.

?MTYPE(discover,    1);
?MTYPE(offer,       2);
?MTYPE(request,     3);
?MTYPE(decline,     4);
?MTYPE(ack,         5);
?MTYPE(nck,         6);
?MTYPE(release,     7);
?MTYPE(inform,      8);
?MTYPE(force_renew, 9).

decode_message_type(Id) ->
    message_type(decode, Id).

encode_message_type(Id) ->
    message_type(encode, Id).

valid_request(#dhcp_package{message_type = discover}) ->
    true;
valid_request(P = #dhcp_package{message_type = request}) ->
    case get_option(dhcp_server_identifier, P) of
        undefined ->
            false;
        _ ->
            true
    end;
valid_request(#dhcp_package{message_type = decline}) ->
    true;
valid_request(#dhcp_package{message_type = release}) ->
    true;
valid_request(#dhcp_package{message_type = inform}) ->
    true;
valid_request(#dhcp_package{message_type = force_renew}) ->
    true;
valid_request(_) ->
    false.

valid_reply(P = #dhcp_package{message_type = offer, yiaddr=Y}) when Y > 0,
                                                                    Y < 16#FFFFFFFF ->
    check_option(P,
                 [ip_address_lease_time,
                  dhcp_server_identifier],
                 [requested_ip_address,
                  parameter_request_list,
                  client_identifier,
                  dhcp_maximum_msg_size]);
valid_reply(P = #dhcp_package{message_type = ack}) ->
    check_option(P,
                 [dhcp_server_identifier],
                 [requested_ip_address,
                  parameter_request_list,
                  client_identifier,
                  dhcp_maximum_msg_size]);
valid_reply(P = #dhcp_package{message_type = nack, options = Options}) ->
    O1 = ordsets:from_list([K || {K, _} <- Options]),
    O2 = ordsets:from_list([dhcp_error_message,
                            client_identifier,
                            vendor_class_identifier,
                            dhcp_server_identifier]),
    check_option(P,
                 [dhcp_server_identifier],
                 []) andalso
        ordsets:is_subset(O1, O2);
valid_reply(_) ->
    false.

check_option(_, [], []) ->
    true;
check_option(P, [Must| R], Forbidden) ->
    (get_option(Must, P) =/= undefined) andalso
        check_option(P, R, Forbidden);
check_option(P, [], [Forbidden | R]) ->
    (get_option(Forbidden, P) =:= undefined) andalso
        check_option(P, [], R).


-ifdef(TEST).
str_decode_test() ->
    ?assertEqual(<<"a">>, decode_string(<<"a", 0:8>>)),
    ?assertEqual(<<"a">>, decode_string(<<$a, 0:8, $c>>)),
    ok.

str_encode_test() ->
    ?assertEqual(<<"a", 0:8>>, encode_string(<<"a">>, 2)),
    ok.

-endif.
