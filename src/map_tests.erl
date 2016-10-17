-module(map_tests).
-author('Daniel Mende <mail@c0decafe.de>').

-include_lib("osmo_ss7/include/sccp.hrl").
-include("ss7MAPer.hrl").

-export([test_hlr/1, test_msc/1, test_smsc/1]).

%~ =========
%~ HLR TESTS
%~ =========

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
    test_pus(Gts, L2),
    ok = sccp_user:unbind_ssn(LocalSsn, undefined),
    L2.

test_sri(Gts, L) ->
    %~ ========
    %~ sendRoutingInfo
    %~ ========
    io:format("~n\e[93;1m# Testing sendRoutingInfo...\n\e[39;49;0m"),
    tcap:send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_sendRoutingInfo(L#loop_dat.msisdn, L#loop_dat.gt_local)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
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
    tcap:send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_sendRoutingInfoForSM(L#loop_dat.msisdn, L#loop_dat.scenter)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
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
                    io:format("\e[91;1mReceived routingInfoForSM, Mobile Station ~w", [ss7_helper:decode_phonenumber(Loc)]);
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
    tcap:send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_sendImsi(L#loop_dat.msisdn)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
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
                    ImsiDec = ss7_helper:decode_imsi(Imsi),
                    io:format("\e[91;1mReceived IMSI ~w~n\e[39;49;0m", [ImsiDec]),
                    L#loop_dat{imsi = ImsiDec}
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
    tcap:send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_sendAuthenticationInfo(L#loop_dat.imsi, Nr)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
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
    tcap:send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_registerSS(L#loop_dat.imsi, L#loop_dat.gt_vlr, L#loop_dat.fnumber)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
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
    tcap:send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_eraseSS(L#loop_dat.imsi, L#loop_dat.gt_vlr)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
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
    tcap:send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_updateLocation(L#loop_dat.imsi, L#loop_dat.gt_local, L#loop_dat.gt_local)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
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
    tcap:send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_anyTimeInterrogation(L#loop_dat.imsi, L#loop_dat.gt_local)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
                {ok, Results} ->
                    io:format("\e[97;1mGot answer for anyTimeInterrogation\n~w\n\e[39;49;0m", [Results]),
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
                io:format("\e[91;1mError decoding anyTimeInterrogation\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for anyTimeInterrogation\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving anyTimeInterrogation\n\e[39;49;0m")
    end,
    L.

test_pms(Gts, L) ->
    %~ ========
    %~ purgeMS
    %~ ========
    io:format("~n\e[93;1m# Testing purgeMS...\n\e[39;49;0m"),
    tcap:send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_purgeMs(L#loop_dat.imsi, L#loop_dat.gt_local)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
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

test_pus(Gts, L) ->
    %~ ========
    %~ processUnstructuredSS
    %~ ========
    io:format("~n\e[93;1m# Testing processUnstructuredSS...\n\e[39;49;0m"),
    tcap:send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_HLR}, map_msgs:create_processUnstructuredSS(L#loop_dat.imsi, L#loop_dat.msisdn, "*100#")),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
                {ok, Results} ->
                    io:format("\e[97;1mGot answer for processUnstructuredSS\n~w\n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}] ->
                    io:format("\e[91;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local]);
                  [{basicROS,
                 {returnResult,
                     {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult',
                         {present,0},
                         {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result',
                             {local,59},
                             {'USSD-Res', _, ResString}}}}}|_] ->
                    io:format("\e[91;1mReceived processUnstructuredSS-Res '~p', processUnstructuredSS is working~n\e[39;49;0m", ['sms_7bit_encoding':from_7bit(ResString)]);
                  _ ->
                    io:format("\e[93;1mNo Error.~n\e[39;49;0m")
                end;
              _->
                io:format("\e[91;1mError decoding processUnstructuredSS\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for processUnstructuredSS\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving processUnstructuredSS\n\e[39;49;0m")
    end,
    L.

%~ =========
%~ MSC TESTS
%~ =========

test_msc(L) ->
    ok = sccp_user:bind_ssn(?SCCP_SSN_HLR),
    Gts = {L#loop_dat.gt_local, L#loop_dat.gt_msc},
    test_psi(Gts, L),
    test_prn(Gts, L),
    test_cl(Gts, L),
    ok = sccp_user:unbind_ssn(?SCCP_SSN_HLR, undefined),
    ok = sccp_user:bind_ssn(?SCCP_SSN_MSC),
    test_sid(Gts, L),
    test_mt_fsm(Gts, L),
    ok = sccp_user:unbind_ssn(?SCCP_SSN_MSC, undefined),
    L.

test_psi(Gts, L) ->
    %~ ========
    %~ provideSubscriberInfo
    %~ ========
    io:format("~n\e[93;1m# Testing provideSubscriberInfo...\n\e[39;49;0m"),
    tcap:send_tcap(L, Gts, {?SCCP_SSN_HLR, ?SCCP_SSN_VLR}, map_msgs:create_provideSubscriberInfo(L#loop_dat.imsi)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
              {ok, Results} ->
                io:format("\e[97;1mGot answer for provideSubscriberInfo~n~w~n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}] ->
                    io:format("\e[91;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local]);
                  [{basicROS,
                    {returnResult,
                      {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult',
                        {present,1},{'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result',
                          {local,70},_}}}}|_] ->
                    io:format("\e[91;1mReceived provideSubscriberInfoRes~n\e[39;49;0m");
                  _ ->
                    io:format("\e[93;1mNo Error.~n\e[39;49;0m")
                end;
              _->
                io:format("\e[91;1mError decoding provideSubscriberInfo\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for provideSubscriberInfo\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving provideSubscriberInfo\n\e[39;49;0m")
    end,
    L.

test_prn(Gts, L) ->
    %~ ========
    %~ provideRoamingNumber
    %~ ========
    io:format("~n\e[93;1m# Testing provideRoamingNumber...\n\e[39;49;0m"),
    tcap:send_tcap(L, Gts, {?SCCP_SSN_HLR, ?SCCP_SSN_VLR}, map_msgs:create_provideRoamingNumber(L#loop_dat.imsi, L#loop_dat.msisdn, L#loop_dat.gt_msc, L#loop_dat.gt_gmsc)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
              {ok, Results} ->
                io:format("\e[97;1mGot answer for provideRoamingNumber~n~w~n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}] ->
                    io:format("\e[91;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local]);
                  [{basicROS,
                    {returnResult,
                      {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult',
                        {present,1},{'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result',
                          {local,4},_}}}}|_] ->
                    io:format("\e[91;1mReceived provideRoamingNumberRes~n\e[39;49;0m");
                  _ ->
                    io:format("\e[93;1mNo Error.~n\e[39;49;0m")
                end;
              _->
                io:format("\e[91;1mError decoding provideRoamingNumber\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for provideRoamingNumber\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving provideRoamingNumber\n\e[39;49;0m")
    end,
    L.

test_cl(Gts, L) ->
    %~ ========
    %~ cancelLocation
    %~ ========
    io:format("~n\e[93;1m# Testing cancelLocation...\n\e[39;49;0m"),
    tcap:send_tcap(L, Gts, {?SCCP_SSN_HLR, ?SCCP_SSN_SGSN}, map_msgs:create_cancelLocation(L#loop_dat.imsi)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
              {ok, Results} ->
                io:format("\e[97;1mGot answer for cancelLocation~n~w~n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}] ->
                    io:format("\e[91;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local]);
                  %~ [{basicROS,
                    %~ {returnResult,
                      %~ {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult',
                        %~ {present,1},{'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result',
                          %~ {local,70},_}}}}|_] ->
                    %~ io:format("\e[91;1mReceived cancelLocationRes~n\e[39;49;0m");
                  _ ->
                    io:format("\e[93;1mNo Error.~n\e[39;49;0m")
                end;
              _->
                io:format("\e[91;1mError decoding cancelLocation\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for cancelLocation\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving cancelLocation\n\e[39;49;0m")
    end,
    L.

test_sid(Gts, L) ->
    %~ ========
    %~ sendIdentification
    %~ ========
    io:format("~n\e[93;1m# Testing sendIdentification...\n\e[39;49;0m"),
    tcap:send_tcap(L, Gts, {?SCCP_SSN_HLR, ?SCCP_SSN_SGSN}, map_msgs:create_sendIdentification()), %L#loop_dat.tmsi
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
              {ok, Results} ->
                io:format("\e[97;1mGot answer for sendIdentification~n~w~n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}] ->
                    io:format("\e[91;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local]);
                  %~ [{basicROS,
                    %~ {returnResult,
                      %~ {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult',
                        %~ {present,1},{'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult_result',
                          %~ {local,70},_}}}}|_] ->
                    %~ io:format("\e[91;1mReceived cancelLocationRes~n\e[39;49;0m");
                  _ ->
                    io:format("\e[93;1mNo Error.~n\e[39;49;0m")
                end;
              _->
                io:format("\e[91;1mError decoding sendIdentification\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for sendIdentification\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving sendIdentification\n\e[39;49;0m")
    end,
    L.

test_mt_fsm(Gts, L) ->
    %~ ========
    %~ mt_forwardSM
    %~ ========
    io:format("~n\e[93;1m# Testing mt_forwardSM...\n\e[39;49;0m"),
    tcap:send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_MSC}, map_msgs:create_mt_forwardSM_v2(map_msgs:create_testSMSPDU_mt(), L#loop_dat.imsi, L#loop_dat.scenter)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
              {ok, Results} ->
                io:format("\e[97;1mGot answer for mt_forwardSM~n~w~n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}] ->
                    io:format("\e[91;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local]);
                  [{basicROS,
                    {returnResult,
                      {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult',
                        {present,1},asn1_NOVALUE}}}|_] ->
                    io:format("\e[91;1mReceived mt_forwardSM-Res~n\e[39;49;0m");
                  _ ->
                    io:format("\e[93;1mNo Error.~n\e[39;49;0m")
                end;
              _->
                io:format("\e[91;1mError decoding mt_forwardSM\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for mt_forwardSM\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving mt_forwardSM\n\e[39;49;0m")
    end,
    L.

%~ ==========
%~ SMSC TESTS
%~ ==========

test_smsc(L) ->
    ok = sccp_user:bind_ssn(?SCCP_SSN_MSC),
    Gts = {L#loop_dat.gt_local, L#loop_dat.gt_smsc},
    test_mo_fsm(Gts, L),
    ok = sccp_user:unbind_ssn(?SCCP_SSN_MSC, undefined),
    L.

test_mo_fsm(Gts, L) ->
    %~ ========
    %~ mo_forwardSM
    %~ ========
    io:format("~n\e[93;1m# Testing mo_forwardSM...\n\e[39;49;0m"),
    tcap:send_tcap(L, Gts, {?SCCP_SSN_MSC, ?SCCP_SSN_MSC}, map_msgs:create_mo_forwardSM_v2(map_msgs:create_testSMSPDU_mo(), L#loop_dat.scenter, L#loop_dat.msisdn)),
    receive
        {sccp, {primitive, 'N', 'UNITDATA', indication, Data}} ->
            case tcap:decode_tcap(Data) of
              {ok, Results} ->
                io:format("\e[97;1mGot answer for mo_forwardSM~n~w~n\e[39;49;0m", [Results]),
                case Results of
                  [{basicROS, {returnError, {_, {present, Present}, {local, Local}, _}}}] ->
                    io:format("\e[91;1mReceived Error: Present ~w, Local ~w~n\e[39;49;0m", [Present, Local]);
                  [{basicROS,
                    {returnResult,
                      {'MapSpecificPDUs_end_components_SEQOF_basicROS_returnResult',
                        {present,1},asn1_NOVALUE}}}|_] ->
                    io:format("\e[91;1mReceived mo_forwardSM-Res~n\e[39;49;0m");
                  _ ->
                    io:format("\e[93;1mNo Error.~n\e[39;49;0m")
                end;
              _->
                io:format("\e[91;1mError decoding mo_forwardSM\n\e[39;49;0m")
            end;
        _->
            io:format("\e[91;1mError no data received for mo_forwardSM\n\e[39;49;0m")
    after 2000 ->
        io:format("\e[91;1mError timeout on receiving mo_forwardSM\n\e[39;49;0m")
    end,
    L.

