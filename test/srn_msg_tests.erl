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
            opts      = [zip_response]
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


zip_test() ->
    ?assertNot(srn_msg:has_opt(?MSG1, msg_zipped)),
    ?assert(size(?MSG1#srn_msg.body) > size(zlib:zip(?MSG1#srn_msg.body))),
    Msg = srn_msg:zip(?MSG1),
    ?assert(srn_msg:has_opt(Msg, msg_zipped)),
    ?assert(size(?MSG1#srn_msg.body) > size(Msg#srn_msg.body)).


zip_unneeded_test() ->
    ?assertNot(srn_msg:has_opt(?MSG1, msg_zipped)),
    Msg = srn_msg:zip(?MSG1),
    ?assert(srn_msg:has_opt(Msg, msg_zipped)),
    ?assertEqual(Msg, srn_msg:zip(Msg)).


zip_useless_test() ->
    ?assertNot(srn_msg:has_opt(?MSG1, msg_zipped)),
    Msg = ?MSG1#srn_msg{body = <<0,1,2,3>>},
    ?assert(size(Msg#srn_msg.body) =< size(zlib:zip(Msg#srn_msg.body))),
    ?assertEqual(Msg, srn_msg:zip(Msg)).


unzip_test() ->
    ?assertNot(srn_msg:has_opt(?MSG1, msg_zipped)),
    Msg = srn_msg:zip(?MSG1),
    ?assert(srn_msg:has_opt(Msg, msg_zipped)),
    ?assertEqual(?MSG1, srn_msg:unzip(Msg)).


unzip_unneeded_test() ->
    ?assertNot(srn_msg:has_opt(?MSG1, msg_zipped)),
    ?assertEqual(?MSG1, srn_msg:unzip(?MSG1)).


has_opt_test() ->
    ?assertNot(srn_msg:has_opt(?MSG1, msg_zipped)),
    ?assert(srn_msg:has_opt(?MSG1, zip_response)),
    ?assertNot(srn_msg:has_opt(?MSG1, sym_encrypted)),
    ?assertNot(srn_msg:has_opt(?MSG1, pub_encrypted)),
    Msg = ?MSG1#srn_msg{hdr = ?MSG1#srn_msg.hdr#srn_hdr{
        opts = [msg_zipped, sym_encrypted, pub_encrypted]
    }},
    ?assert(srn_msg:has_opt(Msg, msg_zipped)),
    ?assertNot(srn_msg:has_opt(Msg, zip_response)),
    ?assert(srn_msg:has_opt(Msg, sym_encrypted)),
    ?assert(srn_msg:has_opt(Msg, pub_encrypted)).


set_opt_test() ->
    Msg = ?MSG1#srn_msg{hdr = ?MSG1#srn_msg.hdr#srn_hdr{
        opts = [msg_zipped, sym_encrypted]
    }},
    Msg1 = srn_msg:set_opt(Msg, zip_response),
    ?assert(srn_msg:has_opt(Msg1, msg_zipped)),
    ?assert(srn_msg:has_opt(Msg1, sym_encrypted)),
    ?assert(srn_msg:has_opt(Msg1, zip_response)),
    Msg2 = srn_msg:set_opt(Msg1, zip_response),
    ?assertEqual(Msg1, Msg2).


unset_opt_test() ->
    Msg = ?MSG1#srn_msg{hdr = ?MSG1#srn_msg.hdr#srn_hdr{
        opts = [msg_zipped, sym_encrypted, zip_response]
    }},
    Msg1 = srn_msg:unset_opt(Msg, zip_response),
    ?assert(srn_msg:has_opt(Msg1, msg_zipped)),
    ?assertNot(srn_msg:has_opt(Msg1, zip_response)),
    ?assert(srn_msg:has_opt(Msg1, sym_encrypted)),
    Msg2 = srn_msg:unset_opt(Msg, sym_encrypted),
    ?assertNot(srn_msg:has_opt(Msg2, sym_encrypted)),
    ?assert(srn_msg:has_opt(Msg2, msg_zipped)),
    ?assert(srn_msg:has_opt(Msg2, zip_response)).


combine_opts_test() ->
    ?assertEqual(0, srn_msg:combine_opts([])),
    ?assertEqual(16#40, srn_msg:combine_opts([pub_encrypted])),
    ?assertEqual(16#10, srn_msg:combine_opts([zip_response])),
    ?assertEqual(16#14, srn_msg:combine_opts([msg_zipped, zip_response])),
    ?assertEqual(16#18, srn_msg:combine_opts([sym_encrypted, zip_response])),
    ?assertEqual(16#1c,
        srn_msg:combine_opts([msg_zipped, sym_encrypted, zip_response])).


extract_opts_test() ->
    ?assertEqual([], srn_msg:extract_opts(0)),
    ?assertEqual([pub_encrypted], srn_msg:extract_opts(16#40)),
    ?assertEqual([zip_response], srn_msg:extract_opts(16#10)),
    ?assertEqual([msg_zipped, zip_response],
        lists:sort(srn_msg:extract_opts(16#14))),
    ?assertEqual([sym_encrypted, zip_response],
        lists:sort(srn_msg:extract_opts(16#18))),
    ?assertEqual([msg_zipped, sym_encrypted, zip_response],
        lists:sort(srn_msg:extract_opts(16#1c))).
