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
         code_change/3]).


-record(st, {session     :: pid(),
             window_size :: pos_integer(),
             queue_len   :: non_neg_integer(),
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
    gen_server:cast(?MODULE, stop).


-spec request/3 :: (binary(), pos_integer(), non_neg_integer()) ->
        {'ok', binary()} | {'error', 'timeout' | 'overloaded'}.

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
                window_size = sirenad_app:get_env(window_size, 1),
                queue_len   = sirenad_app:get_env(queue_len, 0),
                requests    = dict:new(),
                queue       = pqueue:new()
            }};
        {error, Reason} ->
            {stop, Reason}
    end.


terminate(_Reason, St) ->
    srn_session:stop(St#st.session).


handle_call({request, Body, Timeout, Prio}, From, St) ->
    case dict:size(St#st.requests) < St#st.window_size of
        true ->
            Ref = srn_session:request(St#st.session, Body, Timeout),
            {noreply, St#st{
                requests = dict:store(Ref, From, St#st.requests)
            }};
        false ->
            case pqueue:len(St#st.queue) < St#st.queue_len of
                true ->
                    {noreply, St#st{
                        queue = pqueue:in({From, Body, Timeout}, Prio, St#st.queue)
                    }};
                false ->
                    {reply, {error, overloaded}, St}
            end
    end;


handle_call(Request, _From, St) ->
    {stop, {unexpected_call, Request}, St}.


handle_cast({response, Ref, Resp}, St) ->
    From = dict:fetch(Ref, St#st.requests),
    gen_server:reply(From, Resp),
    Dict = dict:erase(Ref, St#st.requests),
    case pqueue:out(St#st.queue) of
        {{value, {From2, Body, Timeout}}, Queue} ->
            Ref2 = srn_session:request(St#st.session, Body, Timeout),
            {noreply, St#st{
                requests = dict:store(Ref2, From2, Dict),
                queue    = Queue
            }};
        {empty, _} ->
            {noreply, St#st{requests = Dict}}
    end;


handle_cast(stop, St) ->
    srn_session:stop(St#st.session),
    {stop, normal, St};


handle_cast(Request, St) ->
    {stop, {unexpected_cast, Request}, St}.


handle_info({'EXIT', Pid, Reason}, #st{session = Pid} = St) ->
    {stop, {session_exit, Reason}, St};


handle_info(Info, St) ->
    {stop, {unexpected_info, Info}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
