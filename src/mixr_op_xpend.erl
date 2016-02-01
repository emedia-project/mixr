% @hidden
-module(mixr_op_xpend).
-compile([{parse_transform, lager_transform}]).
-include("../include/mixr.hrl"). 

-export([action/2]).

action(#request_header{magic = ?REQUEST, 
                       opcode = Opcode, 
                       key_length = KeyLength, 
                       extra_length = ExtraLength, 
                       body_length = BodyLength, 
                       opaque = Opaque}, Body) ->
  if
    size(Body) =:= BodyLength andalso ExtraLength =:= 0 ->
      ValueSize = BodyLength - KeyLength,
      <<Key:KeyLength/binary, Value:ValueSize/binary>> = Body,
      lager:info("[APPEND|PREPEND/~p] ~p :: ~p", [Opcode, Key, Value]),
      Result = if 
                 Opcode =:= ?OP_APPEND orelse Opcode =:= ?OP_APPENDQ ->
                   mixr_store:append(Key, Value);
                 true ->
                   mixr_store:prepend(Key, Value)
               end,
      case Result of
        {ok, CAS} ->
          mixr_operation:response_or_quiet(
            #response_header{opcode = Opcode,
                             opaque = Opaque,
                             cas = CAS});
        not_found ->
          mixr_operation:error_response(Opcode, ?STATUS_KEY_NOT_FOUND, Opaque);
        error ->
          mixr_operation:error_response(Opaque, ?STATUS_INTERNAL_ERROR, Opaque)
      end;
    true ->
      mixr_operation:error_response(Opcode, ?STATUS_INVALID_ARGUMENT, Opaque)
  end.

