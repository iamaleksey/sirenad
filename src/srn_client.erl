-module(srn_client).


-behaviour(gen_server).


%% API exports
-export([start_link/0,
         start_link/1,
         stop/0,
         request/3]).


%% gen_server exports
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         format_status/2]).


-record(req, {from        :: {pid(), reference()},
              body        :: binary(),
              timeout     :: non_neg_integer(),
              retries = 0 :: non_neg_integer()}).


-record(st,  {session     :: pid(),
              session_mod :: atom(),
              is_stopping :: boolean(),
              stop_from   :: {pid(), reference()},
              window_size :: pos_integer(),
              queue_len   :: non_neg_integer(),
              retry_times :: non_neg_integer(),
              requests    :: dict(),
              queue       :: pqueue:pqueue()}).


%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------


-spec start_link/0 :: () -> {'ok', pid()} | {'error', any()}.

start_link() ->
    start_link(srn_session).


-spec start_link/1 :: (atom()) -> {'ok', pid()} | {'error', any()}.

start_link(SessionMod) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, SessionMod, []).


-spec stop/0 :: () -> 'ok'.

stop() ->
    try gen_server:call(?MODULE, stop, infinity) of
        ok -> ok
    catch
        exit:{noproc, _} -> ok
    end.


-spec request/3 :: (binary(), pos_integer(), non_neg_integer()) ->
        {'ok', binary()} | {'error', 'failed' | 'timeout' | 'unavailable'}.

request(Body, Timeout, Prio) ->
    gen_server:call(?MODULE, {request, Body, Timeout, Prio}, infinity).


%% -------------------------------------------------------------------------
%% gen_server callback functions
%% -------------------------------------------------------------------------


init(SessionMod) ->
    process_flag(trap_exit, true),
    log4erl:info("client: initializing"),
    RespFun =
        fun(Pid, Ref, Resp) ->
                gen_server:cast(Pid, {response, Ref, Resp})
        end,
    SessionArgs = [
        {addr,      sirenad_app:get_env(sirena_addr)},
        {port,      sirenad_app:get_env(sirena_port)},
        {client,    self()},
        {resp_fun,  RespFun},
        {client_id, sirenad_app:get_env(client_id)}
    ],
    case SessionMod:start_link(SessionArgs) of
        {ok, Session} ->
            log4erl:info("client: connected to sirena"),
            {ok, #st{
                session     = Session,
                session_mod = SessionMod,
                is_stopping = false,
                window_size = sirenad_app:get_env(window_size, 1),
                queue_len   = sirenad_app:get_env(queue_len, 0),
                retry_times = sirenad_app:get_env(retry_times, 0),
                requests    = dict:new(),
                queue       = pqueue:new()
            }};
        {error, Reason} ->
            log4erl:fatal("client: couldn't connect to sirena"),
            {stop, Reason}
    end.


terminate(Reason, St) ->
    (St#st.session_mod):stop(St#st.session),
    case Reason of
        normal ->
            log4erl:info("client: terminated (normal)");
        _ ->
            log4erl:error("client: terminated (~P)", [Reason, 42])
    end.


handle_call({request, Body, Timeout, Prio}, _From,
        #st{is_stopping = true} = St) ->
    log4erl:warn("client: discarded request [~P, ~w, ~w] (stopping)",
        [Body, 42, Timeout div 1000, Prio]),
    {reply, {error, unavailable}, St};


handle_call({request, Body, Timeout, Prio}, From, St) ->
    Req = #req{from = From, body = Body, timeout = Timeout},
    case dict:size(St#st.requests) < St#st.window_size of
        true ->
            log4erl:debug("client: got request [~P, ~w, ~w]",
                [Body, 42, Timeout div 1000, Prio]),
            make_request(Req, false, St);
        false ->
            case pqueue:len(St#st.queue) < St#st.queue_len of
                true ->
                    log4erl:debug("client: got request [~P, ~w, ~w]",
                        [Body, 42, Timeout div 1000, Prio]),
                    {noreply, St#st{queue = pqueue:in(Req, Prio, St#st.queue)}};
                false ->
                    log4erl:warn(
                        "client: discarded request [~P, ~w, ~w] (queue filled)",
                        [Body, 42, Timeout div 1000, Prio]),
                    {reply, {error, unavailable}, St}
            end
    end;


handle_call(stop, From, St) ->
    log4erl:info("client: stopping"),
    case dict:size(St#st.requests) of
        0 ->
            {stop, normal, ok, St};
        _ ->
            {noreply, St#st{is_stopping = true, stop_from = From}}
    end;


handle_call(Request, _From, St) ->
    {stop, {unexpected_call, Request}, St}.


handle_cast({response, Ref, Resp}, #st{is_stopping = true} = St) ->
    case Resp of
        {ok, Body} ->
            log4erl:debug("client: received ok response [~P]", [Body, 42]);
        {error, Reason} ->
            log4erl:error("client: received error response [~w] (final)",
                [Reason])
    end,
    {Req, St_} = fetch_request_by_ref(Ref, St),
    gen_server:reply(Req#req.from, Resp),
    case dict:size(St_#st.requests) of
        0 ->
            gen_server:reply(St_#st.stop_from, ok),
            {stop, normal, St_};
        _ ->
            {noreply, St_}
    end;


%% Try to retry failed/timed out requests
%% (unless retry_times limit has been exceeded).
%% Here _Reason :: failed | timeout.
handle_cast({response, Ref, {error, Reason} = Resp}, St) ->
    {Req, St_} = fetch_request_by_ref(Ref, St),
    if
        Req#req.retries < St_#st.retry_times ->
            log4erl:warn("client: received error response [~w] (will retry)",
                [Reason]),
            make_request(Req, true, St_);
        true ->
            log4erl:error("client: received error response [~w] (final)",
                [Reason]),
            gen_server:reply(Req#req.from, Resp),
            make_request_from_queue(St_)
    end;


handle_cast({response, Ref, {ok, Body} = Resp}, St) ->
    log4erl:debug("client: received ok response [~P]", [Body, 42]),
    {Req, St_} = fetch_request_by_ref(Ref, St),
    gen_server:reply(Req#req.from, Resp),
    make_request_from_queue(St_);


handle_cast(Request, St) ->
    {stop, {unexpected_cast, Request}, St}.


handle_info({'EXIT', Pid, Reason}, #st{session = Pid} = St) ->
    {stop, {session_exit, Reason}, St};


handle_info(Info, St) ->
    {stop, {unexpected_info, Info}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


format_status(normal, [_PDict, St]) ->
    [{data, [{"State", St}]}];
format_status(terminate, [_PDict, St]) ->
    {
        {state, St#st{requests = undefined, queue = undefined}},
        {stats, [
            {reqs_size, dict:size(St#st.requests)},
            {queue_len, pqueue:len(St#st.queue)}
        ]}
    }.


%% -------------------------------------------------------------------------
%% private functions
%% -------------------------------------------------------------------------


fetch_request_by_ref(Ref, St) ->
    Req = dict:fetch(Ref, St#st.requests),
    St_ = St#st{requests = dict:erase(Ref, St#st.requests)},
    {Req, St_}.


make_request_from_queue(St) ->
    case pqueue:out(St#st.queue) of
        {{value, Req}, Queue} ->
            make_request(Req, false, St#st{queue = Queue});
        {empty, _} ->
            {noreply, St}
    end.


make_request(Req, IsRetry, St) ->
    Ref = (St#st.session_mod):request(St#st.session, Req#req.body, Req#req.timeout),
    Req_ =
        if
            IsRetry -> Req#req{retries = Req#req.retries + 1};
            true    -> Req
        end,
    {noreply, St#st{requests = dict:store(Ref, Req_, St#st.requests)}}.
