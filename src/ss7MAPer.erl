-module(ss7MAPer).
-author('Daniel Mende <mail@c0decafe.de>').

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3, code_change/3, handle_info/2, terminate/2]).

-export([test_hlr/0, test_msc/0, test_smsc/0]).

-include_lib("osmo_ss7/include/osmo_util.hrl").
-include_lib("osmo_ss7/include/osmo_ss7.hrl").
-include_lib("osmo_ss7/include/m3ua.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_map/src/tcap_asn.hrl").
-include("ss7MAPer.hrl").

%% change routing context in m3ua_code.erl => fixed
%% add pointcode to sccp in sccp_codec.erl
%% change network indicator in sccp_scrc.erl => fixed


start_link(Configfile) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Configfile], []).
    
init(Configfile) ->
    {ok, connect(Configfile)}.

connect(Configfile) ->
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
    {gt_gmsc, GT_Gmsc} = lists:keyfind(gt_gmsc, 1, Target),
    {gt_smsc, GT_Smsc} = lists:keyfind(gt_smsc, 1, Target),
    {msisdn, Msisdn} = lists:keyfind(msisdn, 1, Target),
    {imsi, Imsi} = lists:keyfind(imsi, 1, Target),
    Msisdn_enc = ss7_helper:encode_msisdn(?NUMBER_EXTENSION_NONE,
                    ?NUMBER_NATURE_INTERNATIONAL, ?NUMBER_PLAN_ISDN,
                    Msisdn),
    {service_center, SCenter} = lists:keyfind(service_center, 1, Target),
    SCenter_enc = ss7_helper:encode_msisdn(?NUMBER_EXTENSION_NONE,
                    ?NUMBER_NATURE_INTERNATIONAL, ?NUMBER_PLAN_ISDN,
                    SCenter),
    {forward_number, FNumber} = lists:keyfind(forward_number, 1, Target),
    FNumber_enc = ss7_helper:encode_msisdn(?NUMBER_EXTENSION_NONE,
                    ?NUMBER_NATURE_INTERNATIONAL, ?NUMBER_PLAN_ISDN,
                    FNumber),
    %~ error_logger:tty(false),
    % start link server and create linkset
    {ok, SS7linksPid} = ss7_links:start_link(),
    sys:trace(ss7_links, ?TRACE),
    {local_pc, Local_PC} = lists:keyfind(local_pc, 1, M3UA_Config),
    {remote_pc, Remote_PC} = lists:keyfind(remote_pc, 1, M3UA_Config),
    ok = ss7_links:register_linkset(Local_PC, Remote_PC, "test_linkset"),
    %start route server and add route
    {ok, SS7routesPid} = ss7_routes:start_link(),
    %~ sys:trace(ss7_routes, ?TRACE),
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
    %~ sys:trace(ss7_link_m3ua, ?TRACE),
    %~ sys:trace(m3ua_core, ?TRACE),
    % instantiate SCCP routing instance
    {network_ind, Network_Ind} = lists:keyfind(network_ind, 1, M3UA_Config),
	{ok, ScrcPid} = sccp_scrc:start_link([{mtp_tx_action, {callback_fn, fun scrc_tx_to_mtp/2, M3uaPid}},
                {ni, Network_Ind}]),
    sys:trace(sccp_scrc, ?TRACE),
    {ok, _SccpPid} = sccp_user:start_link(),
    sys:trace(sccp_user, ?TRACE),
    io:format("Waiting for M3UA link ...~n"),
    wait_for_link(Link),
    #loop_dat{m3ua_pid = M3uaPid, scrc_pid = ScrcPid, ss7links_pid = SS7linksPid, ss7routes_pid = SS7routesPid, link = Link,
                local_pc = Local_PC, remote_pc = Remote_PC, gt_local = GT_Local, gt_hlr = GT_Hlr, gt_vlr = GT_Vlr, gt_msc = GT_Msc, 
                gt_sgsn = GT_Sgsn, gt_gmsc = GT_Gmsc, gt_smsc = GT_Smsc, msisdn = Msisdn_enc, imsi = hex:hexstr_to_bin(Imsi), scenter = SCenter_enc, fnumber = FNumber_enc}.

wait_for_link(Link) ->
    case ss7_link_m3ua:get_link_state(Link) of
      {ok, down} ->
        io:format("Link not ready, sleeping...~n"),
        timer:sleep(100),
        wait_for_link(Link);
      {ok, active} ->
        {ok};
      _ -> 
        {error}
     end.

handle_cast({test_hlr}, L) ->
    io:format("Testing HLR~n"),
    {noreply, map_tests:test_hlr(L)};

handle_cast({test_msc}, L) ->
    io:format("Testing MSC~n"),
    {noreply, map_tests:test_msc(L)};

handle_cast({test_smsc}, L) ->
    io:format("Testing SMSC~n"),
    {noreply, map_tests:test_smsc(L)}.

handle_call(_, _From, L) ->
    {ok, [], L}.

test_hlr() ->
    gen_server:cast(?MODULE, {test_hlr}).

test_msc() ->
    gen_server:cast(?MODULE, {test_msc}).

test_smsc() ->
    gen_server:cast(?MODULE, {test_smsc}).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_info(Info, S) ->
	error_logger:error_report(["unknown handle_info",
				  {module, ?MODULE},
				  {info, Info}, {state, S}]),
	{noreply, S}.

terminate(Reason, _S) ->
	io:format("terminating ~p with reason ~p", [?MODULE, Reason]),
    ok.

scrc_tx_to_mtp(Prim, Args) ->
	M3uaPid = Args,
	gen_fsm:send_event(M3uaPid, Prim).
    
