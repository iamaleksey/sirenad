-module(srn_client).


-behaviour(gen_server).


%% API exports
-export([start_link/0,
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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec stop/0 :: () -> no_return().

stop() ->
    gen_server:call(?MODULE, stop, infinity).


-spec request/3 :: (binary(), pos_integer(), non_neg_integer()) ->
        {'ok', binary()} | {'error', 'failed' | 'timeout' | 'unavailable'}.

request(Body, Timeout, Prio) ->
    gen_server:call(?MODULE, {request, Body, Timeout, Prio}, infinity).


%% -------------------------------------------------------------------------
%% gen_server callback functions
%% -------------------------------------------------------------------------


init([]) ->
    process_flag(trap_exit, true),
    SessionArgs = [
        {addr,      sirenad_app:get_env(sirena_addr)},
        {port,      sirenad_app:get_env(sirena_port)},
        {client,    self()},
        {client_id, sirenad_app:get_env(client_id)}
    ],
    case srn_session:start_link(SessionArgs) of
        {ok, Session} ->
            {ok, #st{
                session     = Session,
                is_stopping = false,
                window_size = sirenad_app:get_env(window_size, 1),
                queue_len   = sirenad_app:get_env(queue_len, 0),
                retry_times = sirenad_app:get_env(retry_times, 0),
                requests    = dict:new(),
                queue       = pqueue:new()
            }};
        {error, Reason} ->
            {stop, Reason}
    end.


terminate(_Reason, St) ->
    srn_session:stop(St#st.session).


handle_call({request, _, _, _}, _From, #st{is_stopping = true} = St) ->
    {reply, {error, unavailable}, St};


handle_call({request, Body, Timeout, Prio}, From, St) ->
    Req = #req{from = From, body = Body, timeout = Timeout},
    case dict:size(St#st.requests) < St#st.window_size of
        true ->
            make_request(Req, false, St);
        false ->
            case pqueue:len(St#st.queue) < St#st.queue_len of
                true ->
                    {noreply, St#st{queue = pqueue:in(Req, Prio, St#st.queue)}};
                false ->
                    {reply, {error, unavailable}, St}
            end
    end;


handle_call(stop, From, St) ->
    case dict:size(St#st.requests) of
        0 ->
            {stop, normal, ok, St};
        _ ->
            {noreply, St#st{is_stopping = true, stop_from = From}}
    end;


handle_call(Request, _From, St) ->
    {stop, {unexpected_call, Request}, St}.


handle_cast({response, Ref, Resp}, #st{is_stopping = true} = St) ->
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
handle_cast({response, Ref, {error, _Reason} = Resp}, St) ->
    {Req, St_} = fetch_request_by_ref(Ref, St),
    if
        Req#req.retries < St_#st.retry_times ->
            make_request(Req, true, St_);
        true ->
            gen_server:reply(Req#req.from, Resp),
            make_request_from_queue(St_)
    end;


handle_cast({response, Ref, {ok, _Body} = Resp}, St) ->
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
    [
        {state, St#st{requests = undefined, queue = undefined}},
        {stats, [
            {reqs_size, dict:size(St#st.requests)},
            {queue_len, pqueue:len(St#st.queue)}
        ]}
    ].


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
    Ref = srn_session:request(St#st.session, Req#req.body, Req#req.timeout),
    Req_ =
        if
            IsRetry -> Req#req{retries = Req#req.retries + 1};
            true    -> Req
        end,
    {noreply, St#st{requests = dict:store(Ref, Req_, St#st.requests)}}.
