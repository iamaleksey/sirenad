-record(srn_hdr, {id        = 1         :: non_neg_integer(),
                  timestamp = 0         :: non_neg_integer(),
                  status    = ok        :: 'ok' | 'error',
                  client_id = 0         :: non_neg_integer(),
                  key_id    = 0         :: non_neg_integer(),
                  opts = [zip_response] :: [srn_opt()]}).

-record(srn_msg, {hdr :: #srn_hdr{}, body :: binary()}).

-type srn_opt() :: 'msg_zipped' | 'zip_response' | 'sym_encrypted' | 'pub_encrypted'.
-type srn_hdr() :: #srn_hdr{}.
-type srn_msg() :: #srn_msg{}.
