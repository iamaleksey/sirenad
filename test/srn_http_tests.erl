-module(srn_http_tests).


-include_lib("eunit/include/eunit.hrl").


-define(HTTP_PORT, 8889).
-define(REQUEST_TIMEOUT, 10).
-define(ENV, [
    {http_port,       ?HTTP_PORT},
    {request_timeout, ?REQUEST_TIMEOUT}
]).
-define(QUERY,  <<"<sirena><query></query></sirena>">>).
-define(ANSWER, <<"<sirena><answer></answer></sirena>">>).


srn_http_test_() ->
    {foreach,
        fun() ->
                inets:start(),
                mochiweb:start(),
                [ application:set_env(sirenad, K, V) || {K, V} <- ?ENV ],
                srn_http:start_link(),
                {ok, Pid} = gen_server_mock:new(),
                unlink(Pid),
                register(srn_client, Pid),
                Pid
        end,

        fun(Mock) ->
                case is_process_alive(Mock) of
                    true  -> unregister(srn_client);
                    false -> ignore
                end,
                srn_http:stop(),
                gen_server_mock:stop(Mock)
        end,

        [
            {with, [fun test_405/1]},
            {with, [fun test_403/1]},
            {with, [fun test_502/1]},
            {with, [fun test_503/1]},
            {with, [fun test_504/1]},
            {with, [fun test_500/1]},
            {with, [fun test_200_no_headers/1]},
            {with, [fun test_200_with_headers/1]}
        ]
    }.


test_405(_Mock) ->
    ?assertMatch({ok, {{_, 405, _}, _, _}}, get("/", [])).


test_403(_Mock) ->
    ?assertMatch({ok, {{_, 403, _}, _, _}}, post("/wrong", [], ?QUERY)).


test_502(Mock) ->
    gen_server_mock:expect_call(Mock,
        fun({request, ?QUERY, ?REQUEST_TIMEOUT * 1000, 0}, _From, St) ->
                {ok, {error, failed}, St}
        end
    ),
    ?assertMatch({ok, {{_, 502, _}, _, _}}, post("/", [], ?QUERY)),
    gen_server_mock:assert_expectations(Mock).


test_503(Mock) ->
    gen_server_mock:expect_call(Mock,
        fun({request, ?QUERY, ?REQUEST_TIMEOUT * 1000, 0}, _From, St) ->
                {ok, {error, overloaded}, St}
        end
    ),
    ?assertMatch({ok, {{_, 503, _}, _, _}}, post("/", [], ?QUERY)),
    gen_server_mock:assert_expectations(Mock).


test_504(Mock) ->
    gen_server_mock:expect_call(Mock,
        fun({request, ?QUERY, ?REQUEST_TIMEOUT * 1000, 0}, _From, St) ->
                {ok, {error, timeout}, St}
        end
    ),
    ?assertMatch({ok, {{_, 504, _}, _, _}}, post("/", [], ?QUERY)),
    gen_server_mock:assert_expectations(Mock).


test_500(Mock) ->
    gen_server_mock:stop(Mock),
    ?assertMatch({ok, {{_, 500, _}, _, _}}, post("/", [], ?QUERY)).


test_200_no_headers(Mock) ->
    gen_server_mock:expect_call(Mock,
        fun({request, ?QUERY, ?REQUEST_TIMEOUT * 1000, 0}, _From, St) ->
                {ok, {ok, ?ANSWER}, St}
        end
    ),
    ?assertMatch({ok, {{_, 200, _}, _, _}}, post("/", [], ?QUERY)),
    gen_server_mock:assert_expectations(Mock).


test_200_with_headers(Mock) ->
    gen_server_mock:expect_call(Mock,
        fun({request, ?QUERY, 5 * 1000, 9}, _From, St) ->
                {ok, {ok, ?ANSWER}, St}
        end
    ),
    ?assertMatch({ok, {{_, 200, _}, _, _}},
        post("/", [{"X-Timeout", "5"}, {"X-Priority", "9"}], ?QUERY)),
    gen_server_mock:assert_expectations(Mock).


%% -------------------------------------------------------------------------
%% private functions
%% -------------------------------------------------------------------------


get(Path, Headers) ->
    Req = {lists:concat(["http://localhost:", ?HTTP_PORT, Path]), Headers},
    httpc:request(get, Req, [], []).


post(Path, Headers, Body) ->
    Req = {
        lists:concat(["http://localhost:", ?HTTP_PORT, Path]),
        Headers,
        "application/xml",
        Body
    },
    httpc:request(post, Req, [], []).
