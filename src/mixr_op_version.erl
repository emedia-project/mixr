% @hidden
-module(mixr_op_version).
-compile([{parse_transform, lager_transform}]).
-include("../include/mixr.hrl"). 

-export([action/1]).

action(#request_header{magic = ?REQUEST, 
                       opcode = ?OP_VERSION, 
                       opaque = Opaque}) ->
  lager:info("[VERSION]"),
  mixr_operation:response(
    #response_header{opcode = ?OP_VERSION, 
                     opaque = Opaque, 
                     body_length = size(?MIXR_VERSION)},
    <<>>,
    <<>>,
    ?MIXR_VERSION).
