-module(base64).
-export([encode/1]).

-define(BASE64_TAB, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/").

encode(Input) when is_list(Input) ->
    encode(unicode:characters_to_binary(Input));
encode(Input) when is_binary(Input) ->
    encode(Input, <<>>).

encode(<<>>, Output) ->
    Output;
encode(Input, Output) when size(Input) =:= 1 ->
    <<I1>> = Input,
    O1 = misc:index(?BASE64_TAB, (I1 bsr 2)),
    O2 = misc:index(?BASE64_TAB, ((I1 band 2#11) bsl 4)),
    O3 = $=,
    O4 = $=,
    encode(<<>>, <<Output/binary, O1, O2, O3, O4>>);
encode(Input, Output) when size(Input) =:= 2 ->
    <<I1, I2>> = Input,
    O1 = misc:index(?BASE64_TAB, (I1 bsr 2)),
    O2 = misc:index(?BASE64_TAB, (((I1 band 2#11) bsl 4) bor (I2 bsr 4))),
    O3 = misc:index(?BASE64_TAB, ((I2 band 2#1111) bsl 2)),
    O4 = $=,
    encode(<<>>, <<Output/binary, O1, O2, O3, O4>>);
encode(Input, Output) ->
    <<I1, I2, I3, IRest/binary>> = Input,
    O1 = misc:index(?BASE64_TAB, (I1 bsr 2)),
    O2 = misc:index(?BASE64_TAB, (((I1 band 2#11) bsl 4) bor (I2 bsr 4))),
    O3 = misc:index(?BASE64_TAB, (((I2 band 2#1111) bsl 2) bor (I3 bsr 6))),
    O4 = misc:index(?BASE64_TAB, (I3 band 2#111111)),
    encode(IRest, <<Output/binary, O1, O2, O3, O4>>).
