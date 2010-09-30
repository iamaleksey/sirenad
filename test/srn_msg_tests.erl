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


is_zipped_test() ->
    ?assertEqual(false,
        srn_msg:is_zipped(?MSG#srn_msg{hdr = #srn_hdr{flags = 0}})),
    ?assertEqual(true,
        srn_msg:is_zipped(?MSG#srn_msg{hdr = #srn_hdr{flags = 16#04}})).


is_sym_encrypted_test() ->
    ?assertEqual(false,
        srn_msg:is_sym_encrypted(?MSG#srn_msg{hdr = #srn_hdr{flags = 0}})),
    ?assertEqual(true,
        srn_msg:is_sym_encrypted(?MSG#srn_msg{hdr = #srn_hdr{flags = 16#08}})).


is_pub_encrypted_test() ->
    ?assertEqual(false,
        srn_msg:is_pub_encrypted(?MSG#srn_msg{hdr = #srn_hdr{flags = 0}})),
    ?assertEqual(true,
        srn_msg:is_pub_encrypted(?MSG#srn_msg{hdr = #srn_hdr{flags = 16#40}})).


flags_test() ->
    ?assertEqual(0, srn_msg:flags([])),
    ?assertEqual(16#08, srn_msg:flags([sym_encrypted])),
    ?assertEqual(16#10, srn_msg:flags([zip_response])),
    ?assertEqual(16#14, srn_msg:flags([msg_zipped, zip_response])),
    ?assertEqual(16#50, srn_msg:flags([zip_response, pub_encrypted])),
    ?assertEqual(16#54, srn_msg:flags([msg_zipped, zip_response, pub_encrypted])).
