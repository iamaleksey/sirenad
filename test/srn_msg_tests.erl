-module(srn_msg_tests).


-include_lib("eunit/include/eunit.hrl").
-include("srn_msg.hrl").


-define(MSG1,
    #srn_msg{
        hdr = #srn_hdr{
            id        = 4294967293,
            timestamp = 4294967294,
            status    = ok,
            client_id = 65535,
            key_id    = 4294967295,
            flags     = 16#10
        },
        body = <<"<sirena></sirena>">>
    }).

-define(MSG2, ?MSG1#srn_msg{hdr = ?MSG1#srn_msg.hdr#srn_hdr{status = error}}).

-define(BIN1,
    <<17:32, 4294967294:32, 4294967293:32, 0:256, 65535:16, 16#10, 0,
        4294967295:32, 0:384, "<sirena></sirena>">>).

-define(BIN2,
    <<17:32, 4294967294:32, 4294967293:32, 0:256, 65535:16, 16#10, 1,
        4294967295:32, 0:384, "<sirena></sirena>">>).


encode_test() ->
    ?assertEqual(?BIN1, srn_msg:encode(?MSG1)),
    ?assertEqual(?BIN2, srn_msg:encode(?MSG2)).


decode_test() ->
    ?assertEqual({?MSG1, <<>>}, srn_msg:decode(?BIN1)),
    ?assertEqual({?MSG2, <<>>}, srn_msg:decode(?BIN2)).


decode_with_nest_test() ->
    % header + body + part of the next message.
    Next = <<17:32, 4294967294:32>>,
    ?assertEqual({?MSG1, Next}, srn_msg:decode(list_to_binary([?BIN1, Next]))).


decode_incomplete1_test() ->
    % incomplete header.
    ?assertEqual(incomplete, srn_msg:decode(binary:part(?BIN1, {0, 100 - 1}))).


decode_incomplete2_test() ->
    % complete header + incomplete body.
    ?assertEqual(incomplete, srn_msg:decode(binary:part(?BIN1, {0, 100 + 16}))).


decode_error_test() ->
    % invalid header.
    ?assertEqual(error, srn_msg:decode(<<1:32, 1:32, 1:32, 1, 0:1024>>)).


zip_test() ->
    ?assertNot(srn_msg:is_zipped(?MSG1)),
    ?assert(size(?MSG1#srn_msg.body) > size(zlib:zip(?MSG1#srn_msg.body))),
    Msg = srn_msg:zip(?MSG1),
    ?assert(srn_msg:is_zipped(Msg)),
    ?assert(size(?MSG1#srn_msg.body) > size(Msg#srn_msg.body)).


zip_unneeded_test() ->
    ?assertNot(srn_msg:is_zipped(?MSG1)),
    Msg = srn_msg:zip(?MSG1),
    ?assert(srn_msg:is_zipped(Msg)),
    ?assertEqual(Msg, srn_msg:zip(Msg)).


zip_useless_test() ->
    ?assertNot(srn_msg:is_zipped(?MSG1)),
    Msg = ?MSG1#srn_msg{body = <<0,1,2,3>>},
    ?assert(size(Msg#srn_msg.body) =< size(zlib:zip(Msg#srn_msg.body))),
    ?assertEqual(Msg, srn_msg:zip(Msg)).


unzip_test() ->
    ?assertNot(srn_msg:is_zipped(?MSG1)),
    Msg = srn_msg:zip(?MSG1),
    ?assert(srn_msg:is_zipped(Msg)),
    ?assertEqual(?MSG1, srn_msg:unzip(Msg)).


unzip_unneeded_test() ->
    ?assertNot(srn_msg:is_zipped(?MSG1)),
    ?assertEqual(?MSG1, srn_msg:unzip(?MSG1)).


is_zipped_test() ->
    ?assertEqual(false,
        srn_msg:is_zipped(?MSG1#srn_msg{hdr = #srn_hdr{flags = 0}})),
    ?assertEqual(true,
        srn_msg:is_zipped(?MSG1#srn_msg{hdr = #srn_hdr{flags = 16#04}})).


is_sym_encrypted_test() ->
    ?assertEqual(false,
        srn_msg:is_sym_encrypted(?MSG1#srn_msg{hdr = #srn_hdr{flags = 0}})),
    ?assertEqual(true,
        srn_msg:is_sym_encrypted(?MSG1#srn_msg{hdr = #srn_hdr{flags = 16#08}})).


is_pub_encrypted_test() ->
    ?assertEqual(false,
        srn_msg:is_pub_encrypted(?MSG1#srn_msg{hdr = #srn_hdr{flags = 0}})),
    ?assertEqual(true,
        srn_msg:is_pub_encrypted(?MSG1#srn_msg{hdr = #srn_hdr{flags = 16#40}})).


flags_test() ->
    ?assertEqual(0, srn_msg:flags([])),
    ?assertEqual(16#08, srn_msg:flags([sym_encrypted])),
    ?assertEqual(16#10, srn_msg:flags([zip_response])),
    ?assertEqual(16#14, srn_msg:flags([msg_zipped, zip_response])),
    ?assertEqual(16#50, srn_msg:flags([zip_response, pub_encrypted])),
    ?assertEqual(16#54, srn_msg:flags([msg_zipped, zip_response, pub_encrypted])).
