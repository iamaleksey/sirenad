-module(srn_http).


%% API exports
-export([start_link/0,
         stop/0]).


%% internal exports
-export([handle_request/1]).


%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------


-spec start_link/0 :: () -> {'ok', pid()} | {'error', any()}.

start_link() ->
    mochiweb_http:start([
        {name, ?MODULE},
        {port, sirenad_app:get_env(http_port)},
        {loop, fun ?MODULE:handle_request/1}
    ]).


-spec stop/0 :: () -> no_return().

stop() ->
    mochiweb_http:stop(?MODULE).


%% -------------------------------------------------------------------------
%% request handling
%% -------------------------------------------------------------------------


handle_request(Req) ->
    Method = Req:get(method),
    Path = Req:get(path),
    if
        Method =/= 'POST' ->
            Req:respond({405, [], <<"405 Method Not Allowed">>});
        Path =/= "/" ->
            Req:respond({403, [], <<"403 Forbidden">>});
        true ->
            Body = Req:recv_body(10 * 1024 * 1024),
            Timeout = Req:get_header_value("X-Timeout"),
            Priority = Req:get_header_value("X-Priority"),
            handle_request(Req, Body, Timeout, Priority)
    end.


handle_request(Req, Body, Timeout, Priority) ->
    T =
        try list_to_integer(Timeout) of
            Secs -> Secs
        catch
            _:_ -> sirenad_app:get_env(request_timeout)
        end,
    P =
        try list_to_integer(Priority) of
            Level -> Level
        catch
            _:_ -> 0
        end,
    try srn_client:request(Body, T * 1000, P) of
        {ok, RespBody} ->
            Req:ok({"application/xml", RespBody});
        {error, failed} ->
            Req:respond({502, [], <<"502 Bad Gateway">>});
        {error, unavailable} ->
            Req:respond({503, [], <<"503 Service Unavailable">>});
        {error, timeout} ->
            Req:respond({504, [], <<"504 Gateway Timeout">>})
    catch
        _:_ ->
            Req:respond({500, [], <<"500 Internal Server Error">>})
    end.
