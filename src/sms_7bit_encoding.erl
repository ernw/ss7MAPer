-module(sms_7bit_encoding).

-author("dawid.figiel@gmail.com").
-compile(export_all).

%% 7-bit encoding 
%% ---------------------------------------------------------------------
%% Function for converting string to 7-bit encoding according to:
%% GSM 03.38 Version 5.3.0
%% ---------------------------------------------------------------------
%% Initial Function call: to_7bit(String).
%% ---------------------------------------------------------------------
%% Input:  String containing only ASCII characters
%% ---------------------------------------------------------------------
%% Output: Binary encoded String 
%% ---------------------------------------------------------------------
to_7bit([]) -> <<>>;
to_7bit(String) -> to_7bit(list_to_binary(String),<<>>,1).

to_7bit(<<Char1:8>>,<<>>,_Cntr) -> <<<<Char1>>/binary>>;
to_7bit(<<_Char1:8>>,Out,8) -> Out;
to_7bit(<< Char1:8>>,Out,7) ->
%%    << Out/binary,<<((Char1 bsr 6) bor 26)>>/binary >>; 
    << Out/binary,<<((Char1 bsr 6) bor 64)>>/binary >>; 
to_7bit(<<Char1:8>>,Out,Cntr) ->    
    << Out/binary,<<(Char1 bsr (Cntr - 1))>>/binary >>; 
to_7bit(<<_Char:8,In/binary>>,Out,8)->
    to_7bit(In,Out,1);
to_7bit(<<Char1:8,Char2:8,In/binary>>,Out,Cntr)->
    SRChar1 = Char1 bsr (Cntr - 1),
    NewChar1  = <<Char2:Cntr,SRChar1:(8-Cntr)>>,
    to_7bit(<<<<Char2>>/binary,In/binary>>,<<Out/binary,NewChar1/binary>>,Cntr+1).

%% 7-bit decoding 
%% ---------------------------------------------------------------------
%% Function for converting 7-bit encoding to String according to:
%% GSM 03.38 Version 5.3.0
%% ---------------------------------------------------------------------
%% Initial Function call: from_7bit(SevenBitEncodedBinary).
%% ---------------------------------------------------------------------
%% Input: Binary encoded String 
%% ---------------------------------------------------------------------
%% Output:  String containing only ASCII characters
%% ---------------------------------------------------------------------
from_7bit(List) when is_list(List) -> from_7bit(list_to_binary(List));
from_7bit(<<>>) -> [];
from_7bit(Bin) ->
    from_7bit(Bin,<<>>,<<>>,1).

%%from_7bit(<<>>,<<13>>,Out,8) ->
from_7bit(<<>>,<<32>>,Out,8) ->
    binary_to_list(Out);
from_7bit(<<>>,<<CharN>>,Out,8) ->
    binary_to_list(<<Out/binary,CharN:8>>);
from_7bit(<<>>,<<0>>,Out,_Cntr) ->
    binary_to_list(Out);
from_7bit(<<>>,<<CharN>>,Out,Cntr) ->
    binary_to_list(<<Out/binary,0:(9-Cntr),CharN:(Cntr-1)>>);
from_7bit(<<CharN:1,CharO:7,In/binary>>,<<CharI>>,Out,8) ->
    from_7bit(In,<<CharN>>,<<Out/binary,0:1,CharI:7,0:1,CharO:7>>,2);
from_7bit(<<Byte:8,In/binary>>,<<>>,<<>>,1) ->
    CharN = Byte bsr 7,
    from_7bit(In,<<CharN>>,<<(Byte band 127)>>,2);
from_7bit(<<Byte:8,In/binary>>,<<CharI>>,Out,Cntr) ->
    Char = (Byte bsl (Cntr - 1)) bor CharI,
    CharN = Byte bsr (8 - Cntr),
    from_7bit(In,<<CharN>>,<<Out/binary,0:1,Char:7>>,Cntr+1).
