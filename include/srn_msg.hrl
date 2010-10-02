-record(srn_hdr, {id                    :: non_neg_integer(),
                  timestamp             :: pos_integer(),
                  status = ok           :: 'ok' | 'error',
                  client_id             :: non_neg_integer(),
                  key_id                :: non_neg_integer(),
                  opts = [zip_response] :: [srn_opt()]}).

-record(srn_msg, {hdr :: #srn_hdr{}, body :: binary()}).

-type srn_opt() :: 'msg_zipped' | 'zip_response' | 'sym_encrypted' | 'pub_encrypted'.
-type srn_hdr() :: #srn_hdr{}.
-type srn_msg() :: #srn_msg{}.
