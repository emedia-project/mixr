-module(mixr_op_get).
-include("../include/mixr.hrl").

-export([action/3]).
-export([find/2, save_if_needed/2]).

action(Policy, #request_header{magic = ?REQUEST,
                              opcode = Opcode,
                              key_length = KeyLength,
                              extra_length = 0,
                              body_length = BodyLength,
                              opaque = Opaque,
                              cas = 0}, Key) ->
  if
    size(Key) =:= BodyLength andalso KeyLength =:= BodyLength ->
      lager:info("[GET/~p] ~p", [Opcode, Key]),
      case find(Policy, Key) of
        {_, {Key, Value, CAS, _, Flags}} = Data ->
          _ = save_if_needed(Policy, Data),
          Key1 = if
                   Opcode =:= ?OP_GETK ; Opcode =:= ?OP_GETKQ ->
                     Key;
                   true ->
                     <<>>
                 end,
          Flags1 = <<Flags:32>>,
          mixr_operation:response(
            #response_header{
               opcode = ?OP_GET,
               extra_length = size(Flags1),
               body_length = size(Key1) + size(Value) + size(Flags1),
               opaque = Opaque,
               cas = CAS}, Flags1, Key1, Value);
        not_found ->
          mixr_operation:error_response_or_quiet(Opcode, ?STATUS_KEY_NOT_FOUND, Opaque);
        _ ->
          mixr_operation:error_response(Opaque, ?STATUS_INTERNAL_ERROR, Opaque)
      end;
    true ->
      mixr_operation:error_response(Opcode, ?STATUS_INVALID_ARGUMENT, Opaque)
  end.

find(local, Key) ->
  case mixr_store:find(Key) of
    {ok, Data} -> {local, Data};
    _ -> not_found
  end;
find(Policy, Key) ->
  do_find(Policy, Key, mixr_discover:servers_nodes(), find(local, Key)).

do_find(_, _, [], Result) -> Result;
do_find(first, _, _, Result) when Result =/= not_found -> Result;
do_find(Policy, Key, [Node|Rest], not_found) ->
  do_find(Policy, Key, Rest, rpc:call(Node, mixr_store, find, [Key]));
do_find(Policy, Key, [Node|Rest], {_, {_, _, CAS, _, _}} = Current) ->
  case rpc:call(Node, mixr_store, find, [Key]) of
    {ok, {_, _, CAS1, _, _}} = Result ->
      case {CAS1 > CAS, Policy} of
        R when R =:= {true, higher_cas}; R =:= {false, lower_cas} ->
          do_find(Policy, Key, Rest, Result);
        _ ->
          do_find(Policy, Key, Rest, Current)
      end;
    _ ->
      do_find(Policy, Key, Rest, Current)
  end.

save_if_needed(Policy, {ok, {Key, Value, CAS, Expiration, Flags}}) when Policy =:= first_s ;
                                                                        Policy =:= higher_cas_s ;
                                                                        Policy =:= lower_cas_s ->
  case mixr_store:save(Key, Value, CAS, Expiration, Flags) of
    error -> lager:error("[GET] Failed to save data locally");
    _ -> lager:info("[GET] Data save locally")
  end;
save_if_needed(_, _) -> ok.

