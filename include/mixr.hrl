-define(MIXR_VERSION, mixr:version()).
-define(MIXR_DEFAULT_IP, undefined).
-define(MIXR_DEFAULT_SERVER_PORT, 11212).
-define(MIXR_DEFAULT_SERVER_ENABLE, true).
-define(MIXR_DEFAULT_DISCOVER_ENABLE, true).
-define(MIXR_DEFAULT_DISCOVER_IP, "226.0.0.1").
-define(MIXR_DEFAULT_DISCOVER_PORT, 6969).
-define(MIXR_DEFAULT_DISCOVER_MULTICAST_TTL, 1).
-define(MIXR_DEFAULT_SEARCH_POLICY, local).
-define(MIXR_DEFAULT_STORE, mixr_mem_store).
-define(MIXR_DEFAULT_REST_ENABLE, false).
-define(MIXR_DEFAULT_REST_PORT, 21212).
-define(MIXR_DEFAULT_PLUGINS, []).

-define(REQUEST, 128).
-define(RESPONSE, 129).

-define(OP_GET, 0). 
-define(OP_SET, 1). 
-define(OP_ADD, 2). 
-define(OP_REPLACE, 3). 
-define(OP_DELETE, 4). 
-define(OP_INCREMENT, 5). 
-define(OP_DECREMENT, 6). 
-define(OP_QUIT, 7). 
-define(OP_FLUSH, 8). 
-define(OP_GETQ, 9). 
-define(OP_NO_OP, 10).
-define(OP_VERSION, 11).
-define(OP_GETK, 12).
-define(OP_GETKQ, 13).
-define(OP_APPEND, 14).
-define(OP_PREPEND, 15).
-define(OP_STAT, 16).
-define(OP_SETQ, 17).
-define(OP_ADDQ, 18).
-define(OP_REPLACEQ, 19).
-define(OP_DELETEQ, 20).
-define(OP_INCREMENTQ, 21).
-define(OP_DECREMENTQ, 22).
-define(OP_QUITQ, 23).
-define(OP_FLUSHQ, 24).
-define(OP_APPENDQ, 25).
-define(OP_PREPENDQ, 26).
-define(OP_VERBOSITY, 27).
-define(OP_TOUCH, 28).
-define(OP_GAT, 29).
-define(OP_GATQ, 30).
-define(OP_SASL_LIST_MECHS, 32).
-define(OP_SASL_AUTH, 33).
-define(OP_SASL_STEP, 34).
-define(OP_RGET, 48).
-define(OP_RSET, 49).
-define(OP_RSETQ, 50).
-define(OP_RAPPEND, 51).
-define(OP_RAPPENDQ, 52).
-define(OP_RPREPEND, 53).
-define(OP_RPREPENDQ, 54).
-define(OP_RDELETE, 55).
-define(OP_RDELETEQ, 56).
-define(OP_RINCR, 57).
-define(OP_RINCRQ, 58).
-define(OP_RDECR, 59).
-define(OP_RDECRQ, 60).
-define(OP_SET_VBUCKET, 61).
-define(OP_GET_VBUCKET, 62).
-define(OP_DEL_VBUCKET, 63).
-define(OP_TAP_CONNECT, 64).
-define(OP_TAP_MUTATION, 65).
-define(OP_TAP_DELETE, 66).
-define(OP_TAP_FLUSH, 67).
-define(OP_TAP_OPAQUE, 68).
-define(OP_TAP_VBUCKET_SET, 69).
-define(OP_TAP_CHECKPOINT_START, 70).
-define(OP_TAP_CHECKPOINT_END, 71).

-define(QUIET(O), 
        O =:= ?OP_GETQ orelse 
        O =:= ?OP_GETKQ orelse
        O =:= ?OP_SETQ orelse
        O =:= ?OP_ADDQ orelse
        O =:= ?OP_REPLACEQ orelse
        O =:= ?OP_DELETEQ orelse
        O =:= ?OP_INCREMENTQ orelse
        O =:= ?OP_DECREMENTQ orelse
        O =:= ?OP_QUITQ orelse
        O =:= ?OP_FLUSHQ orelse
        O =:= ?OP_APPENDQ orelse
        O =:= ?OP_PREPENDQ orelse
        O =:= ?OP_GATQ orelse
        O =:= ?OP_RSETQ orelse
        O =:= ?OP_RAPPENDQ orelse
        O =:= ?OP_RPREPENDQ orelse
        O =:= ?OP_RDELETEQ orelse
        O =:= ?OP_RINCRQ orelse
        O =:= ?OP_RDECRQ).

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

