%%   The contents of this file are subject to the Mozilla Public License
%%   Version 1.1 (the "License"); you may not use this file except in
%%   compliance with the License. You may obtain a copy of the License at
%%   http://www.mozilla.org/MPL/
%%
%%   Software distributed under the License is distributed on an "AS IS"
%%   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%   License for the specific language governing rights and limitations
%%   under the License.
%%
%%   The Original Code is RabbitMQ.
%%
%%   The Initial Developers of the Original Code are LShift Ltd,
%%   Cohesive Financial Technologies LLC, and Rabbit Technologies Ltd.
%%
%%   Portions created before 22-Nov-2008 00:00:00 GMT by LShift Ltd,
%%   Cohesive Financial Technologies LLC, or Rabbit Technologies Ltd
%%   are Copyright (C) 2007-2008 LShift Ltd, Cohesive Financial
%%   Technologies LLC, and Rabbit Technologies Ltd.
%%
%%   Portions created by LShift Ltd are Copyright (C) 2007-2010 LShift
%%   Ltd. Portions created by Cohesive Financial Technologies LLC are
%%   Copyright (C) 2007-2010 Cohesive Financial Technologies
%%   LLC. Portions created by Rabbit Technologies Ltd are Copyright
%%   (C) 2007-2010 Rabbit Technologies Ltd.
%%
%%   All Rights Reserved.
%%
%%   Contributor(s): ______________________________________.
%%

%% Priority queues have essentially the same interface as ordinary
%% queues, except that a) there is an in/3 that takes a priority, and
%% b) we have only implemented the core API we need.
%%
%% Priorities should be integers - the higher the value the higher the
%% priority - but we don't actually check that.
%%
%% in/2 inserts items with priority 0.
%%
%% We optimise the case where a priority queue is being used just like
%% an ordinary queue. When that is the case we represent the priority
%% queue as an ordinary queue. We could just call into the 'queue'
%% module for that, but for efficiency we implement the relevant
%% functions directly in here, thus saving on inter-module calls and
%% eliminating a level of boxing.
%%
%% When the queue contains items with non-zero priorities, it is
%% represented as a sorted kv list with the inverted Priority as the
%% key and an ordinary queue as the value. Here again we use our own
%% ordinary queue implemention for efficiency, often making recursive
%% calls into the same function knowing that ordinary queues represent
%% a base case.


-module(pqueue).

-export([new/0, is_queue/1, is_empty/1, len/1, to_list/1, in/2, in/3,
         in_r/2, in_r/3, out/1, join/2, filter/2]).

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-type(priority() :: integer()).
-type(squeue() :: {queue, [any()], [any()]}).
-type(pqueue() ::  squeue() | {pqueue, [{priority(), squeue()}]}).

-spec(new/0 :: () -> pqueue()).
-spec(is_queue/1 :: (any()) -> boolean()).
-spec(is_empty/1 :: (pqueue()) -> boolean()).
-spec(len/1 :: (pqueue()) -> non_neg_integer()).
-spec(to_list/1 :: (pqueue()) -> [{priority(), any()}]).
-spec(in/2 :: (any(), pqueue()) -> pqueue()).
-spec(in/3 :: (any(), priority(), pqueue()) -> pqueue()).
-spec(in_r/2 :: (any(), pqueue()) -> pqueue()).
-spec(in_r/3 :: (any(), priority(), pqueue()) -> pqueue()).
-spec(out/1 :: (pqueue()) -> {empty | {value, any()}, pqueue()}).
-spec(join/2 :: (pqueue(), pqueue()) -> pqueue()).
-spec(filter/2 :: (fun((term()) -> boolean() | list()), pqueue()) -> pqueue().

-endif.

%%----------------------------------------------------------------------------

new() ->
    {queue, [], []}.

is_queue({queue, R, F}) when is_list(R), is_list(F) ->
    true;
is_queue({pqueue, Queues}) when is_list(Queues) ->
    lists:all(fun ({P, Q}) -> is_integer(P) andalso is_queue(Q) end,
              Queues);
is_queue(_) ->
    false.

is_empty({queue, [], []}) ->
    true;
is_empty(_) ->
    false.

len({queue, R, F}) when is_list(R), is_list(F) ->
    length(R) + length(F);
len({pqueue, Queues}) ->
    lists:sum([len(Q) || {_, Q} <- Queues]).

to_list({queue, In, Out}) when is_list(In), is_list(Out) ->
    [{0, V} || V <- Out ++ lists:reverse(In, [])];
to_list({pqueue, Queues}) ->
    [{-P, V} || {P, Q} <- Queues, {0, V} <- to_list(Q)].

in(Item, Q) ->
    in(Item, 0, Q).

in(X, 0, {queue, [_] = In, []}) ->
    {queue, [X], In};
in(X, 0, {queue, In, Out}) when is_list(In), is_list(Out) ->
    {queue, [X|In], Out};
in(X, Priority, _Q = {queue, [], []}) ->
    in(X, Priority, {pqueue, []});
in(X, Priority, Q = {queue, _, _}) ->
    in(X, Priority, {pqueue, [{0, Q}]});
in(X, Priority, {pqueue, Queues}) ->
    P = -Priority,
    {pqueue, case lists:keysearch(P, 1, Queues) of
                 {value, {_, Q}} ->
                     lists:keyreplace(P, 1, Queues, {P, in(X, Q)});
                 false ->
                     lists:keysort(1, [{P, {queue, [X], []}} | Queues])
             end}.

in_r(Item, Q) ->
    in_r(Item, 0, Q).

in_r(X, 0, {queue, [], [_] = Out}) ->
    {queue, Out, [X]};
in_r(X, 0, {queue, In, Out}) when is_list(In), is_list(Out) ->
    {queue, In, [X|Out]};
in_r(X, Priority, _Q = {queue, [], []}) ->
    in_r(X, Priority, {pqueue, []});
in_r(X, Priority, Q = {queue, _, _}) ->
    in_r(X, Priority, {pqueue, [{0, Q}]});
in_r(X, Priority, {pqueue, Queues}) ->
    P = -Priority,
    {pqueue, case lists:keysearch(P, 1, Queues) of
                 {value, {_, Q}} ->
                     lists:keyreplace(P, 1, Queues, {P, in_r(X, Q)});
                 false ->
                     lists:keysort(1, [{P, {queue, [], [X]}} | Queues])
             end}.

out({queue, [], []} = Q) ->
    {empty, Q};
out({queue, [V], []}) ->
    {{value, V}, {queue, [], []}};
out({queue, [Y|In], []}) ->
    [V|Out] = lists:reverse(In, []),
    {{value, V}, {queue, [Y], Out}};
out({queue, In, [V]}) when is_list(In) ->
    {{value,V}, r2f(In)};
out({queue, In,[V|Out]}) when is_list(In) ->
    {{value, V}, {queue, In, Out}};
out({pqueue, [{P, Q} | Queues]}) ->
    {R, Q1} = out(Q),
    NewQ = case is_empty(Q1) of
               true -> case Queues of
                           []           -> {queue, [], []};
                           [{0, OnlyQ}] -> OnlyQ;
                           [_|_]        -> {pqueue, Queues}
                       end;
               false -> {pqueue, [{P, Q1} | Queues]}
           end,
    {R, NewQ}.

join(A, {queue, [], []}) ->
    A;
join({queue, [], []}, B) ->
    B;
join({queue, AIn, AOut}, {queue, BIn, BOut}) ->
    {queue, BIn, AOut ++ lists:reverse(AIn, BOut)};
join(A = {queue, _, _}, {pqueue, BPQ}) ->
    {Pre, Post} = lists:splitwith(fun ({P, _}) -> P < 0 end, BPQ),
    Post1 = case Post of
                []                        -> [ {0, A} ];
                [ {0, ZeroQueue} | Rest ] -> [ {0, join(A, ZeroQueue)} | Rest ];
                _                         -> [ {0, A} | Post ]
            end,
    {pqueue, Pre ++ Post1};
join({pqueue, APQ}, B = {queue, _, _}) ->
    {Pre, Post} = lists:splitwith(fun ({P, _}) -> P < 0 end, APQ),
    Post1 = case Post of
                []                        -> [ {0, B} ];
                [ {0, ZeroQueue} | Rest ] -> [ {0, join(ZeroQueue, B)} | Rest ];
                _                         -> [ {0, B} | Post ]
            end,
    {pqueue, Pre ++ Post1};
join({pqueue, APQ}, {pqueue, BPQ}) ->
    {pqueue, merge(APQ, BPQ, [])}.

merge([], BPQ, Acc) ->
    lists:reverse(Acc, BPQ);
merge(APQ, [], Acc) ->
    lists:reverse(Acc, APQ);
merge([{P, A}|As], [{P, B}|Bs], Acc) ->
    merge(As, Bs, [ {P, join(A, B)} | Acc ]);
merge([{PA, A}|As], Bs = [{PB, _}|_], Acc) when PA < PB ->
    merge(As, Bs, [ {PA, A} | Acc ]);
merge(As = [{_, _}|_], [{PB, B}|Bs], Acc) ->
    merge(As, Bs, [ {PB, B} | Acc ]).

filter(Fun, {pqueue, Queues}) when is_function(Fun, 1), is_list(Queues) ->
    Filtered = [{P, filter(Fun, Q)} || {P, Q} <- Queues],
    case [{P, Q} || {P, {queue, In, Out} = Q} <- Filtered, In /= [] orelse Out /= []] of
        []   -> {queue, [], []};
        Some -> {pqueue, Some}
    end;

filter(Fun, {queue, R0, F0}) when is_function(Fun, 1), is_list(R0), is_list(F0) ->
    F = filter_f(Fun, F0),
    R = filter_r(Fun, R0),
    if R =:= [] ->
	    f2r(F);
       F =:= [] ->
	    r2f(R);
       true ->
	    {queue, R, F}
    end.

filter_f(_, []) ->
    [];
filter_f(Fun, [X|F]) ->
    case Fun(X) of
	true ->
	    [X|filter_f(Fun, F)];
	false ->
	    filter_f(Fun, F);
	L when is_list(L) ->
	    L++filter_f(Fun, F)
    end.

filter_r(_, []) ->
    [];
filter_r(Fun, [X|R0]) ->
    R = filter_r(Fun, R0),
    case Fun(X) of
	true ->
	    [X|R];
	false ->
	    R;
	L when is_list(L) ->
	    lists:reverse(L, R)
    end.

-compile({inline, [{r2f,1},{f2r,1}]}).

r2f([])      -> {queue, [], []};
r2f([_] = R) -> {queue, [], R};
r2f([X,Y])   -> {queue, [X], [Y]};
r2f([X,Y|R]) -> {queue, [X,Y], lists:reverse(R, [])}.

f2r([])      -> {queue, [], []};
f2r([_] = F) -> {queue, F, []};
f2r([X,Y])   -> {queue, [Y], [X]};
f2r([X,Y|F]) -> {queue, lists:reverse(F, []), [X,Y]}.
