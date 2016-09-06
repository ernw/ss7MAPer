-module(ss7test_app).
-author('Daniel Mende <mail@c0decafe.de>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-compile([export_all]).     % for debugging

-include_lib("osmo_ss7/include/osmo_util.hrl").
-include_lib("osmo_ss7/include/osmo_ss7.hrl").
-include_lib("osmo_ss7/include/m3ua.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/isup.hrl").
-include_lib("osmo_map/src/tcap_asn.hrl").
-include("ss7test_app.hrl").

-define(TRACE, false).

-record(loop_dat, {
	 scrc_pid,
	 m3ua_pid,
     ss7links_pid,
     ss7routes_pid,
     link,
     local_pc,
     remote_pc,
     gt_local,
     gt_hlr,
     gt_vlr,
     gt_msc,
     gt_sgsn,
     msisdn,
     imsi,
     scenter,
     fnumber
	}).

%% change routing context in m3ua_code.erl => fixed
%% add pointcode to sccp in sccp_codec.erl
%% change network indicator in sccp_scrc.erl => fixed


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_, Configfile) ->
    LoopDat = init(Configfile),
    spawn(ss7test_app, loop, [LoopDat]).

stop(_State) ->
    ok.

%% ===================================================================

init(Configfile) ->
    Config = case file:consult(Configfile) of
        {ok, C} ->
            C;
        {error, E} ->
            io:fwrite("\e[91;1mError reading config file ~p:~n~p~n\e[39;49;0m", [Configfile, E]),
            exit(config_error)
        end,
    {sctp, SCTP_Config} = lists:keyfind(sctp, 1, Config),
    {m3ua, M3UA_Config} = lists:keyfind(m3ua, 1, Config),
    {sccp, Sccp} = lists:keyfind(sccp, 1, Config),
    {target, Target} = lists:keyfind(target, 1, Config),
    {gt_local, GT_Local} = lists:keyfind(gt_local, 1, Sccp),
    {gt_hlr, GT_Hlr} = lists:keyfind(gt_hlr, 1, Target),
    {gt_vlr, GT_Vlr} = lists:keyfind(gt_vlr, 1, Target),
    {gt_msc, GT_Msc} = lists:keyfind(gt_msc, 1, Target),
    {gt_sgsn, GT_Sgsn} = lists:keyfind(gt_sgsn, 1, Target),
    {msisdn, Msisdn} = lists:keyfind(msisdn, 1, Target),
    {imsi, Imsi} = lists:keyfind(imsi, 1, Target),
    Msisdn_enc = ss7test_helper:encode_msisdn(?NUMBER_EXTENSION_NONE,
                    ?NUMBER_NATURE_INTERNATIONAL, ?NUMBER_PLAN_ISDN,
                    Msisdn),
    {service_center, SCenter} = lists:keyfind(service_center, 1, Target),
    SCenter_enc = ss7test_helper:encode_msisdn(?NUMBER_EXTENSION_NONE,
                    ?NUMBER_NATURE_INTERNATIONAL, ?NUMBER_PLAN_ISDN,
                    SCenter),
    {forward_number, FNumber} = lists:keyfind(forward_number, 1, Target),
    FNumber_enc = ss7test_helper:encode_msisdn(?NUMBER_EXTENSION_NONE,
                    ?NUMBER_NATURE_INTERNATIONAL, ?NUMBER_PLAN_ISDN,
                    FNumber),
    % start link server and create linkset
    {ok, SS7linksPid} = ss7_links:start_link(),
    sys:trace(ss7_links, ?TRACE),
    {local_pc, Local_PC} = lists:keyfind(local_pc, 1, M3UA_Config),
    {remote_pc, Remote_PC} = lists:keyfind(remote_pc, 1, M3UA_Config),
    ok = ss7_links:register_linkset(Local_PC, Remote_PC, "test_linkset"),
    %start route server and add route
    {ok, SS7routesPid} = ss7_routes:start_link(),
    ok = ss7_routes:create_route(Remote_PC, 16#ffff, "test_linkset"),
    % create m3ua link
    {local_ip, Local_IP} = lists:keyfind(local_ip, 1, SCTP_Config),
    {local_port, Local_Port} = lists:keyfind(local_port, 1, SCTP_Config),
    Local = #sigtran_peer{ip = Local_IP, port = Local_Port},
    {remote_ip, Remote_IP} = lists:keyfind(remote_ip, 1, SCTP_Config),
    {remote_port, Remote_Port} = lists:keyfind(remote_port, 1, SCTP_Config),
    Remote = #sigtran_peer{ip = Remote_IP, port = Remote_Port},
    {asp_id, Asp_ID} = lists:keyfind(asp_id, 1, M3UA_Config),
    {route_ctx, Route_CTX} = lists:keyfind(route_ctx, 1, M3UA_Config),
    {net_appearance, Net_Appearance} = lists:keyfind(net_appearance, 1, M3UA_Config),
    Link = #sigtran_link{type = m3ua, name = "test_link", linkset_name = "test_linkset",
                sls = 0, local = Local, remote = Remote, asp_id = Asp_ID,
                route_ctx = Route_CTX, net_app = Net_Appearance},
    {ok, M3uaPid} = ss7_link_m3ua:start_link(Link),
    % instantiate SCCP routing instance
    {network_ind, Network_Ind} = lists:keyfind(network_ind, 1, M3UA_Config),
	{ok, ScrcPid} = sccp_scrc:start_link([{mtp_tx_action, {callback_fn, fun scrc_tx_to_mtp/2, M3uaPid}},
                {ni, Network_Ind}]),
    sys:trace(sccp_scrc, ?TRACE),
    {ok, _SccpPid} = sccp_user:start_link(),
    sys:trace(sccp_user, ?TRACE),
    io:format("Waiting for M3UA link comming up...~n"),
    wait_for_link(Link),
    #loop_dat{m3ua_pid = M3uaPid, scrc_pid = ScrcPid, ss7links_pid = SS7linksPid, ss7routes_pid = SS7routesPid, link = Link,
                local_pc = Local_PC, remote_pc = Remote_PC, gt_local = GT_Local, gt_hlr = GT_Hlr, gt_vlr = GT_Vlr, gt_msc = GT_Msc, 
                gt_sgsn = GT_Sgsn, msisdn = Msisdn_enc, imsi = Imsi, scenter = SCenter_enc, fnumber = FNumber_enc}.

wait_for_link(Link) ->
    case ss7_link_m3ua:get_link_state(Link) of
      {ok, down} ->
        timer:sleep(100),
        wait_for_link(Link);
      {ok, active} ->
        {ok};
      _ -> 
        {error}
     end.

loop(L) ->
    receive
		{sccp, Prim} ->
            {primitive, 'N', 'UNITDATA', indication, Data} = Prim,
            {sccp_msg, _, ProtData} = Data,
            {user_data, _UserData} = lists:keyfind(user_data, 1, ProtData),
			%~ io:format("Rx: ~p~n", [map:decode('MapSpecificPDUs', UserData)]),
			loop(L);
        {send, {Gts, {Lssn, Rssn}}, Data} ->
            Tlssn = case Lssn of
                hlr  -> ?SCCP_SSN_HLR;
                vlr  -> ?SCCP_SSN_VLR;
                msc  -> ?SCCP_SSN_MSC;
                sgsn -> ?SCCP_SSN_SGSN;
                cap  -> ?SCCP_SSN_CAP;
                _    -> ?SCCP_SSN_UNKNOWN
            end,
            Trssn = case Rssn of
                hlr  -> ?SCCP_SSN_HLR;
                vlr  -> ?SCCP_SSN_VLR;
                msc  -> ?SCCP_SSN_MSC;
                sgsn -> ?SCCP_SSN_SGSN;
                cap  -> ?SCCP_SSN_CAP;
                _    -> ?SCCP_SSN_UNKNOWN
            end,
            send_tcap(L, Gts, {Tlssn, Trssn}, Data),
            loop(L);
        {test_hlr} ->
            io:format("Testing HLR~n"),
            test_hlr(L),
            loop(L);
        {link_state} ->
            io:format("~p~n", [ss7_link_m3ua:get_link_state(L#loop_dat.link)]),
            loop(L);
		Stop ->
            M3uaPid = L#loop_dat.m3ua_pid,
            %~ ScrcPid = L#loop_dat.scrc_pid,
            SS7linksPid = L#loop_dat.ss7links_pid,
            %~ SS7routesPid = L#loop_dat.ss7routes_pid,
			io:format("Received ~p~n", [Stop]),
            sccp_user:stop(),
            sccp_scrc:stop(),
            %~ ss7_link_m3ua:stop(),
            gen_server:cast(M3uaPid, stop),
            ss7_routes:stop(),
            %~ ss7_links:stop(),
            gen_server:cast(SS7linksPid, stop),
			exit(stop_received)
	end.

test_hlr(L) ->
    LocalSsn = ?SCCP_SSN_MSC,
    ok = sccp_user:bind_ssn(LocalSsn),
    Gts = {L#loop_dat.gt_local, L#loop_dat.gt_hlr},
    test_sri(Gts, L),
    test_srifs(Gts, L),
    L2 = test_si(Gts, L),
    test_sai(Gts, L2, 100),
    test_sai(Gts, L2, 10),
    test_sai(Gts, L2, 5),
    test_rss(Gts, L2),
    test_ess(Gts, L2),
    test_ul(Gts, L2),
    test_ati(Gts, L2),
    test_pms(Gts, L2),
    ok = sccp_user:unbind_ssn(LocalSsn, undefined).

test_sri(Gts, L) ->
    %~ ========
    %~ sendRoutingInfo
    %~ ========
    io:format("~n\e[93;1m# Testing sendRoutingInfo...\n\e[39;49;0m"),
    send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_sendRoutingInfo(L#loop_dat.msisdn, L#loop_dat.gt_local)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case decode_tcap(Data) of
              {ok, Results} ->
                io:format("\e[97;1mGot answer for sendRoutingInfo~n~w~n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}] ->
                    io:format("\e[91;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local]);
                  [{basicROS,
                    {returnResult,
                      {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult',
                        {present,1},{'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result',
                          {local,22},_}}}}|_] ->
                    io:format("\e[91;1mReceived SendRoutingInfoRes~n\e[39;49;0m");
                  _ ->
                    io:format("\e[93;1mNo Error.~n\e[39;49;0m")
                end;
              _->
                io:format("\e[91;1mError decoding SendRoutingInfo\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for SendRoutingInfo\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving SendRoutingInfo\n\e[39;49;0m")
    end,
    L.

test_srifs(Gts, L) ->
    %~ ========
    %~ sendRoutingInfoForSM
    %~ ========
    io:format("~n\e[93;1m# Testing sendRoutingInfoForSM...\n\e[39;49;0m"),
    send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_sendRoutingInfoForSM(L#loop_dat.msisdn, L#loop_dat.scenter)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case decode_tcap(Data) of
              {ok, Results} ->
                io:format("\e[97;1mGot answer for sendRoutingInfoForSM\n~w\n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}|_] ->
                    case {Present, Local} of
                      {1, 6} ->
                        io:format("\e[91;1mSubscriber is absent~n\e[39;49;0m");
                      _ ->
                        io:format("\e[92;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local])
                    end;
                  [{basicROS,
                   {returnResult,
                    {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult',
                     {present,1},{'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result',
                      {local,45}, Res}}}}|_] ->
                    {'RoutingInfoForSM-Res',_,
                      {'LocationInfoWithLMSI',Loc,_,_,_,_},_,_} = Res,
                    io:format("\e[91;1mReceived routingInfoForSM, Mobile Station ~w", [ss7test_helper:decode_phonenumber(Loc)]);
                  _ ->
                    io:format("\e[93;1mNo Error.~n\e[39;49;0m")
                end;
              _->
                io:format("\e[91;1mError decoding SendRoutingInfoForSM\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for SendRoutingInfoForSM\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving SendRoutingInfoForSM\n\e[39;49;0m")
    end,
    L.

test_si(Gts, L) ->
    %~ ========
    %~ sendImsi
    %~ ========
    io:format("~n\e[93;1m# Testing sendImsi...\n\e[39;49;0m"),
    send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_sendImsi(L#loop_dat.msisdn)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case decode_tcap(Data) of
              {ok, Results} ->
                io:format("\e[97;1mGot answer for sendImsi\n~w\n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}] ->
                    io:format("\e[91;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local]),
                    L;
                  [{basicROS,
                    {returnResult,
                      {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult',
                        {present,1},
                          {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result',
                            {local,58},
                              Imsi }}}}|_] ->
                    io:format("\e[91;1mReceived IMSI ~w~n\e[39;49;0m", [ss7test_helper:decode_imsi(Imsi)]),
                    L#loop_dat{imsi = Imsi}
                end;
              _->
                io:format("\e[91;1mError decoding sendImsi\n\e[39;49;0m"),
                L
            end;
        _->
            io:format("\e[91;1mError no data received for sendImsi\n\e[39;49;0m"),
            L
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving sendImsi\n\e[39;49;0m"),
        L
    end.

test_sai(Gts, L, Nr) ->
    %~ ========
    %~ sendAuthenticationInfo
    %~ ========
    io:format("~n\e[93;1m# Testing sendAuthenticationInfo...\n\e[39;49;0m"),
    send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_sendAuthenticationInfo(L#loop_dat.imsi, Nr)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case decode_tcap(Data) of
                {ok, Results} ->
                    io:format("\e[97;1mGot answer for sendAuthenticationInfo\n~w\n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}] ->
                    io:format("\e[91;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local]);
                  [{basicROS, {reject, _}}|_] ->
                    if
                      Nr>5 -> io:format("\e[92;1mAsked for ~w (>5) vectors, got rejected~n\e[39;49;0m", [Nr]);
                      Nr<6 -> io:format("\e[91;1mAsked for ~w (<=5) vectors, got rejected~n\e[39;49;0m", [Nr])
                    end;
                  [{basicROS,
                    {returnResult,
                      {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult',
                        {present,1}, {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result',
                          {local,56}, {'SendAuthenticationInfoRes',
                            {quintupletList,ResList},_,_}}}}}|_] ->
                    NrRes = length(ResList),
                    if
                      Nr>5 ->
                        if
                          NrRes>5 -> io:format("\e[91;1mAsked for ~w vectors, got ~w (>5) result vectors~n\e[39;49;0m", [Nr,NrRes])
                        end;
                      Nr<6 ->
                        if
                          NrRes==Nr -> io:format("\e[92;1mAsked for ~w vectors, got ~w (=~w) result vectors~n\e[39;49;0m", [Nr,NrRes,Nr]);
                          true ->     io:format("\e[93;1mAsked for ~w vectors, got ~w (!=~w) result vectors~n\e[39;49;0m", [Nr,NrRes,Nr])
                        end
                    end;
                  _ ->
                    io:format("\e[93;1mNo Error.~n\e[39;49;0m")
                end;
              _->
                io:format("\e[91;1mError decoding sendAuthenticationInfo\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for sendAuthenticationInfo\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving sendAuthenticationInfo\n\e[39;49;0m")
    end,
    L.

test_rss(Gts, L) ->
    %~ ========
    %~ registerSS
    %~ ========
    io:format("~n\e[93;1m# Testing registerSS...\n\e[39;49;0m"),
    send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_registerSS(ss7test_helper:decode_imsi(L#loop_dat.imsi), L#loop_dat.gt_vlr, L#loop_dat.fnumber)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case decode_tcap(Data) of
                {ok, Results} ->
                    io:format("\e[97;1mGot answer for registerSS\n~w\n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}] ->
                    io:format("\e[91;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local]);
                  [{basicROS,
                    {returnResult,
                      {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult',
                        {present,1}, {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result',
                          {local,10},_}}}}|_] ->
                    io:format("\e[91;1mReceived forwardingInfo, registerSS is working~n\e[39;49;0m");
                  _ ->
                    io:format("\e[93;1mNo Error.~n\e[39;49;0m")
                end;
              _->
                io:format("\e[91;1mError decoding registerSS\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for registerSS\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving registerSS\n\e[39;49;0m")
    end,
    L.

test_ess(Gts, L) ->
    %~ ========
    %~ eraseSS
    %~ ========
    io:format("~n\e[93;1m# Testing eraseSS...\n\e[39;49;0m"),
    send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_eraseSS(ss7test_helper:decode_imsi(L#loop_dat.imsi), L#loop_dat.gt_vlr)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case decode_tcap(Data) of
                {ok, Results} ->
                    io:format("\e[97;1mGot answer for eraseSS\n~w\n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}] ->
                    io:format("\e[91;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local]);
                  [{basicROS,
                    {returnResult,
                      {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult',
                        {present,1}, {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result',
                          {local,11},_}}}}|_] ->
                    io:format("\e[91;1mReceived forwardingInfo, eraseSS is working~n\e[39;49;0m");
                  _ ->
                    io:format("\e[93;1mNo Error.~n\e[39;49;0m")
                end;
              _->
                io:format("\e[91;1mError decoding eraseSS\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for eraseSS\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving eraseSS\n\e[39;49;0m")
    end,
    L.

test_ul(Gts, L) ->
    %~ ========
    %~ updateLocation
    %~ ========
    io:format("~n\e[93;1m# Testing updateLocation...\n\e[39;49;0m"),
    send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_updateLocation(L#loop_dat.imsi, L#loop_dat.gt_local, L#loop_dat.gt_local)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case decode_tcap(Data) of
                {ok, Results} ->
                    io:format("\e[97;1mGot answer for updateLocation\n~w\n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}] ->
                    io:format("\e[91;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local]);
                  [{basicROS,{invoke,{'MapSpecificPDUs_continue_components_SEQOF_basicROS_invoke',{present,2},asn1_NOVALUE,{local,7},_}}}|_] ->
                    io:format("\e[91;1mReceived insertSubscriberData, updateLocation is working~n\e[39;49;0m");
                  _ ->
                    io:format("\e[93;1mNo Error.~n\e[39;49;0m")
                end;
              _->
                io:format("\e[91;1mError decoding updateLocation\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for updateLocation\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving updateLocation\n\e[39;49;0m")
    end,
    L.

test_ati(Gts, L) ->
    %~ ========
    %~ anyTimeInterrogation
    %~ ========
    io:format("~n\e[93;1m# Testing anyTimeInterrogation...\n\e[39;49;0m"),
    send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_anyTimeInerrogation(L#loop_dat.imsi, L#loop_dat.gt_local)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case decode_tcap(Data) of
                {ok, Results} ->
                    io:format("\e[97;1mGot answer for anyTimeInerrogation\n~w\n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}] ->
                    case {Present, Local} of
                      {1, 49} ->
                        io:format("\e[92;1manyTimeInterrogation is forbidden~n\e[39;49;0m");
                      _ ->
                        io:format("\e[91;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local])
                    end;
                  _ ->
                    io:format("\e[93;1mNo Error.~n\e[39;49;0m")
                end;
              _->
                io:format("\e[91;1mError decoding anyTimeInerrogation\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for anyTimeInerrogation\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving anyTimeInerrogation\n\e[39;49;0m")
    end,
    L.

test_pms(Gts, L) ->
    %~ ========
    %~ purgeMS
    %~ ========
    io:format("~n\e[93;1m# Testing purgeMS...\n\e[39;49;0m"),
    send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_purgeMs(L#loop_dat.imsi, L#loop_dat.gt_local)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case decode_tcap(Data) of
                {ok, Results} ->
                    io:format("\e[97;1mGot answer for purgeMs\n~w\n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}] ->
                    io:format("\e[91;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local]);
                  [{basicROS,
                    {returnResult,
                      {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult',
                        {present,1},{'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result',
                          {local,67},_}}}}|_] ->
                    io:format("\e[91;1mReceived purgeMS-Res, purgeMS is working~n\e[39;49;0m");
                  _ ->
                    io:format("\e[93;1mNo Error.~n\e[39;49;0m")
                end;
              _->
                io:format("\e[91;1mError decoding purgeMs\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for purgeMs\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving purgeMs\n\e[39;49;0m")
    end,
    L.

scrc_tx_to_mtp(Prim, Args) ->
	M3uaPid = Args,
	gen_fsm:send_event(M3uaPid, Prim).

send_tcap(L, {Lgt, Rgt}, {Sssn, Dssn}, PDU) ->
    CallingP = #sccp_addr{ssn = Sssn, point_code = L#loop_dat.local_pc, 
        global_title = #global_title{gti = ?SCCP_GTI_TT_NP_ENC_NAT, trans_type = ?SCCP_GTI_NO_GT,
            encoding = 0, numbering_plan = 1, phone_number = Lgt,
            nature_of_addr_ind = ?ISUP_ADDR_NAT_INTERNATIONAL}},
	CalledP = #sccp_addr{ssn = Dssn, point_code = L#loop_dat.remote_pc,
        global_title = #global_title{gti = ?SCCP_GTI_TT_NP_ENC_NAT, trans_type = ?SCCP_GTI_NO_GT,
            encoding = 0, numbering_plan = 1, phone_number = Rgt,
            nature_of_addr_ind = ?ISUP_ADDR_NAT_INTERNATIONAL}},
	Opts = [{protocol_class, {1, 8}}, {called_party_addr, CalledP},
		{calling_party_addr, CallingP}, {user_data, PDU}],
    gen_fsm:send_event(L#loop_dat.scrc_pid, osmo_util:make_prim('N','UNITDATA',request,Opts)).

decode_tcap(Data) ->
    {sccp_msg, _, ProtData} = Data,
    {user_data, UserData} = lists:keyfind(user_data, 1, ProtData),
    {ok, TcapData} = map:decode('MapSpecificPDUs', UserData),
    case TcapData of
      {'end', {'MapSpecificPDUs_end', _Transaction, % <<1,1,0,0>>,
        {'EXTERNAL',
          {0,0,17,773,1,1,1},
          _,_, %asn1_NOVALUE,asn1_NOVALUE,
          _Dialog}, Results}} -> {};
      {continue, {'MapSpecificPDUs_continue', _STransaction, _Transaction,
        {'EXTERNAL',
          {0,0,17,773,1,1,1},
          _,_, %asn1_NOVALUE,asn1_NOVALUE,
          _Dialog}, Results}} -> {}
    end,
    {ok, Results}.
