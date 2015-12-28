-module(ss7test_helper).
-author('Daniel Mende <mail@c0decafe.de>').

-export([encode_phonenumber/4, 
         decode_phonenumber/1,
         encode_msisdn/4,
         decode_imsi/1,
         remove_firstN/2,
         tup2bin/1]).

decode_imsi(<<>>) ->
    [];
decode_imsi(Imsi) ->
    <<First:4,Second:4,Rest/bits>> = Imsi,
    Dec = decode_imsi(Rest),
    if
      First==15 -> lists:append([Second], Dec);
      true ->       lists:append([Second,First], Dec)
    end.

%% ===================================================================
%% Phone Number helper
%% ===================================================================

encode_phonenumber(Extension, NatureOfNumber, NumberPlan, Number) ->
    {EncNumber, Digits} = encode_phonenumber(Number),
    <<Digits:8, Extension:1, NatureOfNumber:3, NumberPlan:4, EncNumber/binary>>.
encode_phonenumber([First,Second|Tail]) ->
    {EncNumber, Digits} = encode_phonenumber(Tail),
    {<<Second:4, First:4, EncNumber/binary>>, Digits + 2};
encode_phonenumber([Last]) ->
    {<<15:4, Last:4>>, 1};
encode_phonenumber([]) ->
    {<<>>, 0}.

decode_phonenumber(<<_Extension:1, _NatureOfNumber:1, _NumberPlan:4, Number/binary>>) ->
    decode_imsi(Number).

encode_msisdn(Extension, NatureOfNumber, NumberPlan, Number) ->
    {EncNumber, _} = encode_phonenumber(Number),
    <<Extension:1, NatureOfNumber:3, NumberPlan:4, EncNumber/binary>>.

%% ===================================================================
%% List helper
%% ===================================================================

remove_firstN(_, []) -> [];
remove_firstN(1, [_|T]) -> T;
remove_firstN(N, [_|T]) -> remove_firstN(N-1, T).


tup2bin(Tupel) ->
    binary:list_to_bin([element(I,Tupel) || I <- lists:seq(1,tuple_size(Tupel))]).
