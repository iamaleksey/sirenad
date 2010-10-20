-module(srn_msg).


-include("srn_msg.hrl").


-ifdef(TEST).
-compile(export_all).
-endif.


-export([encode/1, decode/1]).
-export([zip/1, unzip/1]).
-export([has_opt/2]).


%% flags:
-define(MSG_ZIPPED,    16#04).
-define(ZIP_RESPONSE,  16#10).
-define(SYM_ENCRYPTED, 16#08).
-define(PUB_ENCRYPTED, 16#40).

%% mask-to-opt mapping:
-define(OPTS, [
    {?MSG_ZIPPED,    msg_zipped},
    {?ZIP_RESPONSE,  zip_response},
    {?SYM_ENCRYPTED, sym_encrypted},
    {?PUB_ENCRYPTED, pub_encrypted}
]).


%% -------------------------------------------------------------------------
%% encode/decode
%% -------------------------------------------------------------------------


-spec encode/1 :: (srn_msg()) -> binary().

encode(Msg) ->
    #srn_msg{
        hdr = #srn_hdr{
            id        = Id,
            timestamp = Timestamp,
            status    = Status,
            client_id = ClientId,
            key_id    = KeyId,
            opts      = Opts
        },
        body = Body
    } = Msg,
    StatusB = case Status of ok -> 0; error -> 1 end,
    Mask = combine_opts(Opts),
    Len = size(Body),
    <<Len:32, Timestamp:32, Id:32, 0:32/unit:8, ClientId:16, Mask:8, StatusB:8,
        KeyId:32, 0:48/unit:8, Body/binary>>.


-spec decode/1 :: (binary()) -> {srn_msg(), binary()} | 'incomplete'.

decode(Buffer) when size(Buffer) < 100 ->
    incomplete;

decode(<<Len:32, Timestamp:32, Id:32, 0:32/unit:8, ClientId:16, Mask:8, StatusB:8,
         KeyId:32, 0:48/unit:8, Rest/binary>>) ->
    if
        size(Rest) < Len ->
            incomplete;
        true ->
            {Body, Next} = split_binary(Rest, Len),
            {
                #srn_msg{
                    hdr = #srn_hdr{
                        id        = Id,
                        timestamp = Timestamp,
                        status    = case StatusB of 0 -> ok; 1 -> error end,
                        client_id = ClientId,
                        key_id    = KeyId,
                        opts      = extract_opts(Mask)
                    },
                    body = Body
                },
                Next
            }
    end.


%% -------------------------------------------------------------------------
%% zip/unzip
%% -------------------------------------------------------------------------


-spec zip/1 :: (srn_msg()) -> srn_msg().

zip(#srn_msg{body = Body} = Msg) ->
    case has_opt(Msg, msg_zipped) of
        true ->
            Msg; % already zipped somehow.
        false ->
            Zipped = zlib:zip(Body),
            if
                size(Zipped) >= size(Body) ->
                    Msg; % zipping proved to be useless - don't replace body.
                true ->
                    set_opt(Msg#srn_msg{body = Zipped}, msg_zipped)
            end
    end.


-spec unzip/1 :: (srn_msg()) -> srn_msg().

unzip(#srn_msg{body = Body} = Msg) ->
    case has_opt(Msg, msg_zipped) of
        true ->
            unset_opt(Msg#srn_msg{body = zlib:unzip(Body)}, msg_zipped);
        false ->
            Msg % not zipped.
    end.


%% -------------------------------------------------------------------------
%% opt helpers
%% -------------------------------------------------------------------------


-spec has_opt/2 :: (srn_msg(), srn_opt()) -> boolean().

has_opt(#srn_msg{hdr = Hdr}, Opt) ->
    lists:any(fun(O) -> O =:= Opt end, Hdr#srn_hdr.opts).


%% -------------------------------------------------------------------------
%% private functions
%% -------------------------------------------------------------------------


-spec set_opt/2 :: (srn_msg(), srn_opt()) -> srn_msg().

set_opt(#srn_msg{hdr = #srn_hdr{opts = Opts} = Hdr} = Msg, Opt) ->
    Msg#srn_msg{hdr = Hdr#srn_hdr{opts = lists:usort([Opt|Opts])}}.


-spec unset_opt/2 :: (srn_msg(), srn_opt()) -> srn_msg().

unset_opt(#srn_msg{hdr = #srn_hdr{opts = Opts} = Hdr} = Msg, Opt) ->
    Msg#srn_msg{hdr = Hdr#srn_hdr{opts = lists:delete(Opt, lists:usort(Opts))}}.


-spec combine_opts/1 :: ([srn_opt()]) -> non_neg_integer().

combine_opts(Opts) ->
    lists:foldl(
        fun(Opt, Mask) ->
                Mask bor element(1, lists:keyfind(Opt, 2, ?OPTS))
        end, 0, Opts
    ).


-spec extract_opts/1 :: (non_neg_integer()) -> [srn_opt()].

extract_opts(Mask) ->
    lists:foldl(
        fun({Flag, Opt}, Opts) ->
                case Mask band Flag =:= Flag of
                    true  -> [Opt|Opts];
                    false -> Opts
                end
        end, [], ?OPTS
    ).
