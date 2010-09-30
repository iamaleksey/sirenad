-module(srn_msg).


-include("srn_msg.hrl").


-export([encode/1, decode/1]).


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
    Len = size(Body),
    <<Len:32, Timestamp:32, Id:32, 0:256, ClientId:16, Flags:8, Status:8,
        KeyId:32, 0:384, Body/binary>>.


-spec decode/1 :: (binary()) -> {srn_msg(), binary()} | 'incomplete' | 'error'.

decode(Buffer) when size(Buffer) < 100 ->
    incomplete;

decode(<<Len:32, Timestamp:32, Id:32, 0:256, ClientId:16, Flags:8, Status:8,
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
                        status    = Status,
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
