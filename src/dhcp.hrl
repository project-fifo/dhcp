-type message_type() ::
        discover | offer | request | decline |
        ack | nck | release | inform | force_renew.


-type dhcp_op() :: request | reply.
-type dhcp_op_id() :: 1..2.

-type int32() :: 0..4294967295.
-type octet() :: 0..255.
-type short() :: 0..65535.
-type mac() :: {octet(),octet(),octet(),octet(),octet(),octet()}.
-type ip() :: int32().
-type ip_tpl() :: {octet(), octet(), octet(), octet()}.
-type dhcp_flags() :: [broadcast] | [].

%-type zts() :: <<X:integer/8>> when X >=1.
-type null_terminated_string() :: binary().


-type htype() :: ethernet |
                 experiemental_ethernet |
                 ax25 |
                 proteon_token_ring |
                 chaos |
                 ieee802 |
                 arcnet |
                 hyperchannel |
                 lanstar |
                 autonet_short_address |
                 localtalk |
                 localnet |
                 ultra_link |
                 smds |
                 frame_relay |
                 atm16 |
                 hdlc |
                 fibre_channel |
                 atm19 |
                 serial_line |
                 atm21 |
                 mil_std_188_220 |
                 metricom |
                 ieee1394 |
                 mapos |
                 twinaxial |
                 eui64 |
                 hiparp |
                 ip_over_iso_7816_3 |
                 arpsec |
                 ipsec_tunnel |
                 infiniband |
                 cai_tia_102.

-type htype_id() :: 1..33.

-type dhcp_option() :: {subnet_mask, ip()} |
                       {time_offset, int32()} |
                       {router_address, [ip(),...]} |
                       {time_server, [ip(),...]} |
                       {ien_116_name_server, [ip(),...]} |
                       {domain_name_server, [ip(),...]} |
                       {log_server, [ip(),...]} |
                       {quotes_server, [ip(),...]} |
                       {lpr_server, [ip(),...]} |
                       {impress_server, [ip(),...]} |
                       {rlp_server, [ip(),...]} |
                       {hostname, binary()} |
                       {boot_file_size, short()} |
                       {merit_dump_file, binary()} |
                       {domain_name, binary()} |
                       {swap_server, ip()} |
                       {root_path, binary()} |
                       {bootp_extensions_path, binary()} |
                       {ip_forward_enable, byte()} |
                       {source_route_enable, byte()} |
                       {policy_filter, binary()} |
                       {max_datagram_reassembly_sz, short()} |
                       {default_ip_ttl, binary()} |
                       {path_mtu_aging_timeout, int32()} |
                       {path_mtu_plateau_table, [short(), ...]} |
                       {interface_mtu_size, short()} |
                       {all_subnets_are_local, byte()} |
                       {broadcast_address, ip()} |
                       {perform_mask_discovery, byte()} |
                       {provide_mask_to_others, byte()} |
                       {perform_router_discovery, byte()} |
                       {router_solicitation_address, ip()} |
                       {static_routes, [ip(),...]} |
                       {trailer_encapsulation, byte()} |
                       {arp_cache_timeout, int32()} |
                       {ethernet_encapsulation, byte()} |
                       {default_tcp_ttl, byte()} |
                       {keep_alive_interval, int32()} |
                       {keep_alive_garbage, byte()} |
                       {nis_domain_name, binary()} |
                       {nis_servers, [ip(),...]} |
                       {ntp_servers, [ip(),...]} |
                       {vendor, binary()} |
                       {netbios_name_servers, [ip(),...]} |
                       {netbios_dgm_dist_servers, [ip(),...]} |
                       {netbios_node_type, byte()} |
                       {netbios, binary()} |
                       {x_window_font_server, [ip(),...]} |
                       {x_window_display_mgr, [ip(),...]} |
                       {requested_ip_address, ip()} |
                       {ip_address_lease_time, int32()} |
                       {overload, byte()} |
                       {message_type, message_type()} |
                       {dhcp_server_identifier, ip()} |
                       {parameter_request_list, [byte(), ...]} |
                       {dhcp_error_message, binary()} |
                       {dhcp_maximum_msg_size, short()} |
                       {renewal_time, int32()} |
                       {rebinding_time, int32()} |
                       {vendor_class_identifier, binary()} |
                       {client_identifier, binary()} |
                       {netware_domain_name, binary()} |
                       {netware_sub_options, binary()} |
                       {nis_client_domain_name, binary()} |
                       {nis_server_address, ip()} |
                       {tftp_server_name, binary()} |
                       {boot_file_name, binary()} |
                       {home_agent_address, binary()} |
                       {smtp_server_address, [ip(),...]} |
                       {pop3_server_address, [ip(),...]} |
                       {nntp_server_address, [ip(),...]} |
                       {www_server_address, [ip(),...]} |
                       {finger_server_address, [ip(),...]} |
                       {irc_server_address, [ip(),...]} |
                       {streettalk_server_address, [ip(),...]} |
                       {stda_server_address, [ip(),...]} |
                       {user_class, binary()} |
                       {directory_agent, binary()} |
                       {service_scope, binary()} |
                       {rapid_commit, binary()} |
                       {client_fqdn, binary()} |
                       {relay_agent_information, binary()} |
                       {isns, binary()} |
                       {nds_servers, binary()} |
                       {nds_tree_name, binary()} |
                       {nds_context, binary()} |
                       {authentication, binary()} |
                       {client_last_txn_time, binary()} |
                       {associated_ip, binary()} |
                       {client_system, binary()} |
                       {client_ndi, binary()} |
                       {ldap, binary()} |
                       {uuid_guid, binary()} |
                       {user_auth, binary()} |
                       {netinfo_address, binary()} |
                       {netinfo_tag, binary()} |
                       {url, binary()} |
                       {auto_config, byte()} |
                       {name_service_search, binary()} |
                       {subnet_selection_option, binary()} |
                       {domain_search, binary()} |
                       {sip_servers_dhcp_option, binary()} |
                       {classless_static_route, binary()} |
                       {ccc, binary()} |
                       {geoconf_option, binary()} |
                       {v_i_vendor_class, binary()} |
                       {v_i_vendor_specific, binary()} |
                       {etherboot, binary()} |
                       {call_server_ip_address, binary()} |
                       {ethernet_interface, binary()} |
                       {remote_stats_svr_ip_address, binary()} |
                       {ieee_802_1q_l2_priority, binary()} |
                       {ieee_802_1p_vlan_id, binary()} |
                       {diffserv_code_point, binary()} |
                       {http_proxy, binary()} |
                       {cisco_tftp_server_ip_addresses, [ip(),...]}.

-type package_fields() :: op | htype | hlen | hops |
                          xid |
                          secs | flags |
                          ciaddr | yiaddr | siaddr  | giaddr |
                          chaddr | sname | file |
                          options |
                          message_type.
-record(dhcp_package,
        {op = request :: dhcp_op(),
         htype = ethernet :: htype(),
         hlen = 0 :: octet(),
         hops = 0 :: octet(),
         xid = 0 :: int32(),
         secs = 0 :: short(),
         flags = [] :: dhcp_flags(),
         ciaddr = 0 :: ip(),
         yiaddr = 0 :: ip(),
         siaddr = 0 :: ip(),
         giaddr = 0 :: ip(),
         chaddr = {0,0,0,0,0,0} :: mac(),
         sname = <<>> :: binary(),
         file = <<>> :: binary(),
         options = [] :: [dhcp_option()],
         message_type = ack :: message_type()
        }).
