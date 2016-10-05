-module(tcap).
-author('Daniel Mende <mail@c0decafe.de>').

-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/isup.hrl").
-include("ss7MAPer.hrl").

-export([send_tcap/4, decode_tcap/1]).

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
    %~ io:format("Sending N-UNITDATA.req to SCRC~n"),
	%~ io:format("Link is in state ~p~n", [sys:get_status(L#loop_dat.scrc_pid)]),
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
