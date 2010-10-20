-module(srn_session).


-include("srn_msg.hrl").


-behaviour(gen_fsm).


%% API exports
-export([start_link/1,
         stop/1,
         request/3]).


%% gen_fsm exports
-export([init/1,
         terminate/3,
         handle_sync_event/4,
         handle_event/3,
         handle_info/3,
         code_change/4]).


%% internal exports
-export([recv_loop/3]).


-define(MAX_MSG_ID, 16#ffffffff).
-define(INCR_MSG_ID(MsgId),
    if MsgId =:= ?MAX_MSG_ID -> 1; true -> MsgId + 1 end).


-type response() :: {'ok', binary()} | {'error', 'timeout' | 'failed'}.
-type resp_fun() :: fun((pid(), reference(), response()) -> any()).


-record(st, {sock      :: port(),
             recv_loop :: pid(),
             client    :: pid(),
             resp_fun  :: resp_fun(),
             msg_id    :: non_neg_integer(),
             client_id :: non_neg_integer(),
             key_id    :: non_neg_integer(),
             requests  :: ets:tid()}).


%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------


-spec start_link/1 :: (list()) -> {'ok', pid()} | {'error', any()}.

start_link(Args) ->
    gen_fsm:start_link(?MODULE, Args, []).


-spec stop/1 :: (pid()) -> no_return().

stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).


-spec request/3 ::
        (pid(), binary(), non_neg_integer()) -> reference().

request(Pid, Body, Timeout) ->
    Event = {request, Body, Timeout},
    gen_fsm:sync_send_all_state_event(Pid, Event, infinity).


%% -------------------------------------------------------------------------
%% gen_fsm callback functions
%% -------------------------------------------------------------------------


init(Args) ->
    Addr = proplists:get_value(addr, Args),
    Port = proplists:get_value(port, Args),
    case gen_tcp:connect(Addr, Port, [binary]) of
        {ok, Sock} ->
            Loop = proc_lib:spawn_link(?MODULE, recv_loop, [self(), Sock, <<>>]),
            ok = gen_tcp:controlling_process(Sock, Loop),
            {ok, connected, #st{
                sock      = Sock,
                recv_loop = Loop,
                client    = proplists:get_value(client, Args),
                resp_fun  = proplists:get_value(resp_fun, Args),
                msg_id    = 0,
                client_id = proplists:get_value(client_id, Args),
                key_id    = 0,
                requests  = ets:new(requests, [])
            }};
        {error, Reason} ->
            {stop, Reason}
    end.


terminate(_Reason, _Stn, Std) ->
    Std#st.recv_loop ! stop,
    gen_tcp:close(Std#st.sock),
    ets:delete(Std#st.requests).


handle_sync_event({request, Body, Timeout}, _From, Stn, Std) ->
    Ref = make_ref(),
    MsgId = ?INCR_MSG_ID(Std#st.msg_id),
    ok = send_msg(Std#st.sock, Body, MsgId, Std#st.client_id),
    Timer = erlang:start_timer(Timeout, self(), {response_timer, MsgId}),
    ets:insert(Std#st.requests, {MsgId, Ref, Timer}),
    {reply, Ref, Stn, Std#st{msg_id = MsgId}};


handle_sync_event(Event, _From, _Stn, Std) ->
    {stop, {unexpected_sync_event, Event}, Std}.


handle_event({response, Msg}, Stn, Std) ->
    #srn_msg{hdr = #srn_hdr{id = MsgId, status = Status}, body = Body} = Msg,
    case ets:lookup(Std#st.requests, MsgId) of
        [{MsgId, Ref, Timer}] ->
            erlang:cancel_timer(Timer),
            Response =
                case Status of
                    ok    -> {ok, Body};
                    error -> {error, failed}
                end,
            (Std#st.resp_fun)(Std#st.client, Ref, Response),
            ets:delete(Std#st.requests, MsgId);
        [] ->
            ignore
    end,
    {next_state, Stn, Std};


handle_event({sock_closed, Reason}, _Stn, Std) ->
    {stop, {sock_closed, Reason}, Std};


handle_event(stop, _Stn, Std) ->
    {stop, normal, Std};


handle_event(Event, _Stn, Std) ->
    {stop, {unexpected_event, Event}, Std}.


handle_info({timeout, Timer, {response_timer, MsgId}}, Stn, Std) ->
    case ets:lookup(Std#st.requests, MsgId) of
        [{MsgId, Ref, Timer}] ->
            ets:delete(Std#st.requests, MsgId),
            (Std#st.resp_fun)(Std#st.client, Ref, {error, timeout});
        [] ->
            ignore
    end,
    {next_state, Stn, Std};


handle_info(Info, _Stn, Std) ->
    {stop, {unexpected_info, Info}, Std}.


code_change(_OldVsn, Stn, Std, _Extra) ->
    {ok, Stn, Std}.


%% -------------------------------------------------------------------------
%% socket receive loop
%% -------------------------------------------------------------------------


recv_loop(Pid, Sock, Buffer) ->
    receive
        {tcp, Sock, Data} ->
            Buffer_ = handle_data(Pid, list_to_binary([Buffer, Data])),
            ?MODULE:recv_loop(Pid, Sock, Buffer_);
        {tcp_closed, Sock} ->
            gen_fsm:send_all_state_event(Pid, {sock_closed, closed});
        {tcp_error, Sock, Reason} ->
            gen_fsm:send_all_state_event(Pid, {sock_closed, Reason});
        stop ->
            ok
    end.


handle_data(Pid, Data) ->
    case srn_msg:decode(Data) of
        {Msg, Data_} ->
            Msg_ = srn_msg:unzip(Msg),
            gen_fsm:send_all_state_event(Pid, {response, Msg_}),
            handle_data(Pid, Data_);
        incomplete ->
            Data
    end.


%% -------------------------------------------------------------------------
%% private functions
%% -------------------------------------------------------------------------


timestamp() ->
    {MegaSecs, Secs, _} = os:timestamp(),
    MegaSecs * 1000000 + Secs.


send_msg(Sock, Body, Id, ClientId) ->
    Msg = #srn_msg{
        hdr = #srn_hdr{
            id        = Id,
            timestamp = timestamp(),
            client_id = ClientId
        },
        body = Body
    },
    Bin = srn_msg:encode(srn_msg:zip(Msg)),
    gen_tcp:send(Sock, Bin).
