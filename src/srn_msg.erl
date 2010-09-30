-module(srn_msg).


-include("srn_msg.hrl").


-export([encode/1, decode/1]).
-export([is_zipped/1, is_sym_encrypted/1, is_pub_encrypted/1, flags/1]).


%% flags:
-define(MSG_ZIPPED,    16#04).
-define(ZIP_RESPONSE,  16#10).
-define(SYM_ENCRYPTED, 16#08).
-define(PUB_ENCRYPTED, 16#40).


%% -------------------------------------------------------------------------
%% API
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
            flags     = Flags
        },
        body = Body
    } = Msg,
    StatusB = case Status of ok -> 0; error -> 1 end,
    Len = size(Body),
    <<Len:32, Timestamp:32, Id:32, 0:256, ClientId:16, Flags:8, StatusB:8,
        KeyId:32, 0:384, Body/binary>>.


-spec decode/1 :: (binary()) -> {srn_msg(), binary()} | 'incomplete' | 'error'.

decode(Buffer) when size(Buffer) < 100 ->
    incomplete;

decode(<<Len:32, Timestamp:32, Id:32, 0:256, ClientId:16, Flags:8, StatusB:8,
         KeyId:32, 0:384, Rest/binary>>) ->
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
                        flags     = Flags
                    },
                    body = Body
                },
                Next
            }
    end;

decode(_) ->
    error.


%% -------------------------------------------------------------------------
%% Flag helpers
%% -------------------------------------------------------------------------


-spec is_zipped/1 :: (srn_msg()) -> boolean().

is_zipped(Msg) ->
    Msg#srn_msg.hdr#srn_hdr.flags band ?MSG_ZIPPED =:= ?MSG_ZIPPED.


-spec is_sym_encrypted/1 :: (srn_msg()) -> boolean().

is_sym_encrypted(Msg) ->
    Msg#srn_msg.hdr#srn_hdr.flags band ?SYM_ENCRYPTED =:= ?SYM_ENCRYPTED.


-spec is_pub_encrypted/1 :: (srn_msg()) -> boolean().

is_pub_encrypted(Msg) ->
    Msg#srn_msg.hdr#srn_hdr.flags band ?PUB_ENCRYPTED =:= ?PUB_ENCRYPTED.


-spec flags/1 ::
    (['msg_zipped' | 'zip_response' | 'sym_encrypted' | 'pub_encrypted']) ->
        non_neg_integer().

flags(Aliases) ->
    lists:foldl(
        fun(msg_zipped,    Mask) -> Mask bor ?MSG_ZIPPED;
           (zip_response,  Mask) -> Mask bor ?ZIP_RESPONSE;
           (sym_encrypted, Mask) -> Mask bor ?SYM_ENCRYPTED;
           (pub_encrypted, Mask) -> Mask bor ?PUB_ENCRYPTED
        end, 0, Aliases
    ).
