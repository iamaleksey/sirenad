-module(srn_msg_tests).


-include_lib("eunit/include/eunit.hrl").
-include("srn_msg.hrl").


-define(MSG,
    #srn_msg{
        hdr = #srn_hdr{
            id        = 4294967293,
            timestamp = 4294967294,
            status    = 0,
            client_id = 65535,
            key_id    = 4294967295,
            flags     = 16#10
        },
        body = <<"<sirena></sirena>">>
    }).

-define(BIN,
    <<17:32, 4294967294:32, 4294967293:32, 0:256, 65535:16, 16#10, 0,
        4294967295:32, 0:384, "<sirena></sirena>">>).


encode_test() ->
    ?assertEqual(?BIN, srn_msg:encode(?MSG)).


decode_test() ->
    ?assertEqual({?MSG, <<>>}, srn_msg:decode(?BIN)).


decode_with_nest_test() ->
    % header + body + part of the next message.
    Next = <<17:32, 4294967294:32>>,
    ?assertEqual({?MSG, Next}, srn_msg:decode(list_to_binary([?BIN, Next]))).


decode_incomplete1_test() ->
    % incomplete header.
    ?assertEqual(incomplete, srn_msg:decode(binary:part(?BIN, {0, 100 - 1}))).


decode_incomplete2_test() ->
    % complete header + incomplete body.
    ?assertEqual(incomplete, srn_msg:decode(binary:part(?BIN, {0, 100 + 16}))).


decode_error_test() ->
    % invalid header.
    ?assertEqual(error, srn_msg:decode(<<1:32, 1:32, 1:32, 1, 0:1024>>)).
