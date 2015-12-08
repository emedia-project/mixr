-module(mixr_op_delete).
-compile([{parse_transform, lager_transform}]).
-include("../include/mixr.hrl"). 

-export([action/2]).

action(#request_header{magic = ?REQUEST, 
                       opcode = Opcode, 
                       key_length = KeyLength, 
                       extra_length = 0, 
                       body_length = BodyLength, 
                       opaque = Opaque}, Key) ->
  if 
    size(Key) =:= BodyLength andalso KeyLength =:= BodyLength ->
      lager:info("[DELETE/~p] ~p", [Opcode, Key]),
      case mixr_store:delete(Key) of
        ok -> 
          mixr_operation:response_or_quiet(
            #response_header{opcode = Opcode,
                             opaque = Opaque});
        not_found ->
          mixr_operation:error_response_or_quiet(Opcode, ?STATUS_KEY_NOT_FOUND, Opaque)
      end;
    true ->
      mixr_operation:error_response(Opcode, ?STATUS_INVALID_ARGUMENT, Opaque)
  end.

