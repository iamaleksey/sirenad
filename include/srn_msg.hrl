-record(srn_hdr, {id          :: pos_integer(),
                  timestamp   :: pos_integer(),
                  status = ok :: 'ok' | 'error',
                  client_id   :: pos_integer(),
                  key_id      :: pos_integer(),
                  flags = 0   :: non_neg_integer()}).

-record(srn_msg, {hdr :: #srn_hdr{}, body :: binary()}).

-type srn_hdr() :: #srn_hdr{}.
-type srn_msg() :: #srn_msg{}.
