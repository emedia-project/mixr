-module(mixr_op_set).
-compile([{parse_transform, lager_transform}]).
-include("../include/mixr.hrl").

-export([action/2]).

action(#request_header{magic = ?REQUEST,
                       opcode = Opcode,
                       key_length = KeyLength,
                       extra_length = ExtraLength,
                       body_length = BodyLength,
                       opaque = Opaque,
                       cas = CAS},
       <<Flags:32, Expiration:32, Body/binary>> = Body1) ->
  if
    size(Body1) =:= BodyLength andalso ExtraLength =:= 8 ->
      ValueSize = BodyLength - ExtraLength - KeyLength,
      <<Key:KeyLength/binary, Value:ValueSize/binary>> = Body,
      lager:info("[SET/~p] ~p = ~p", [Opcode, Key, Value]),
      case set_data(mixr_store:exist(Key, CAS), Opcode) of
        false ->
          mixr_operation:error_response(Opcode, ?STATUS_KEY_EXISTS, Opaque);
        true ->
          NewCAS = mixr_utils:cas(CAS),
          case mixr_store:save(Key, Value, NewCAS, Expiration, Flags) of
            error ->
              mixr_operation:error_response(Opcode, ?STATUS_INTERNAL_ERROR, Opaque);
            {ok, NewCAS} ->
              mixr_operation:response_or_quiet(
                #response_header{opcode = Opcode,
                                 opaque = Opaque,
                                 cas = NewCAS})
          end
      end;
    true ->
      mixr_operation:error_response(Opcode, ?STATUS_INVALID_ARGUMENT, Opaque)
  end.

set_data(_, ?OP_SET) -> true;
set_data(true, ?OP_REPLACE) -> true;
set_data(false, ?OP_ADD) -> true;
set_data(_, ?OP_SETQ) -> true;
set_data(true, ?OP_REPLACEQ) -> true;
set_data(false, ?OP_ADDQ) -> true;
set_data(_, _) -> false.

