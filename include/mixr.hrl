-define(MIXR_VERSION, <<"1.4.15">>).

-define(REQUEST, 128).
-define(RESPONSE, 129).

-define(OP_GET, 0).
-define(OP_SET, 1).
-define(OP_GETQ, 9).
-define(OP_VERSION, 11).
-define(OP_GETK, 12).
-define(OP_GETKQ, 13).

-define(STATUS_NO_ERROR, 0).
-define(STATUS_KEY_NOT_FOUND, 1).
-define(STATUS_KEY_EXISTS, 2).
-define(STATUS_VALUE_TOO_LARGE, 3).
-define(STATUS_INVALID_ARGUMENT, 4).
-define(STATUS_INTEM_NOT_STORED, 5).
-define(STATUS_INCR_DECR_ON_NON_NUMERIC_VALUE, 6).
-define(STATUS_THE_VBUCKET_BELONGS_TO_ANOTHER_SERVER, 7).
-define(STATUS_AUTHENTICATION_ERROR, 8).
-define(STATUS_AUTHENTICATION_CONTINUE, 9).
-define(STATUS_UNKNOWN_COMMAND, 129).
-define(STATUS_OUT_OF_MEMORY, 130).
-define(STATUS_NOT_SUPPORTED, 131).
-define(STATUS_INTERNAL_ERROR, 132).
-define(STATUS_BUSY, 133).
-define(STATUS_TEMPORARY_FAILURE, 134).

-record(request_header, {
          magic = ?REQUEST,
          opcode = 0,
          key_length = 0,
          extra_length = 0,
          data_type = 0,
          vbucket = 0,
          body_length = 0,
          opaque = 0,
          cas = 0}).

-record(response_header, {
          magic = ?RESPONSE,
          opcode = 0,
          key_length = 0,
          extra_length = 0,
          data_type = 0,
          status = ?STATUS_NO_ERROR,
          body_length = 0,
          opaque = 0,
          cas = 0}).

