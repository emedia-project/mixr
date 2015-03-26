-module(mixr_op_get).
-include("../include/mixr.hrl"). 

-export([action/3]).

action(local, #request_header{magic = ?REQUEST, 
                              opcode = Opcode, 
                              key_length = KeyLength, 
                              extra_length = 0, 
                              body_length = BodyLength, 
                              opaque = Opaque,
                              cas = 0}, Key) ->
  if 
    size(Key) =:= BodyLength andalso KeyLength =:= BodyLength ->
      lager:info("[GET/~p] ~p", [Opcode, Key]),
      case mixr_store:find(Key) of
        {ok, {Key, Value, CAS, _, Flags}} ->
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
  end;
action(Policy, Header, Key) ->
  do_action(Policy, Header, Key, mixr_discover:servers_nodes(), [action(local, Header, Key)]).

do_action(_, _, _, [], [Current]) -> Current;
do_action(first, Header, Key, [Node|Rest], [Current]) ->
  case mixr_operation:is_error(Current) of
    {false, Response} -> 
      Response;
    {true, _} ->
      do_action(first, Header, Key, Rest, [rpc:call(Node, ?MODULE, action, [local, Header, Key])])
  end;
do_action(Policy, _, _, [], [Last|Other] = Results) ->
  Results1 = case mixr_operation:is_error(Last) of
               {true, _} -> Other;
               {false, _} -> Results
             end,
  [Final|_] = lists:sort(fun({reply, <<?RESPONSE, 
                                       _, 
                                       _:16, 
                                       _, 
                                       _, 
                                       ?STATUS_NO_ERROR:16, 
                                       _:32, 
                                       _:32, 
                                       CAS1:64, 
                                       _/binary>>},
                             {reply, <<?RESPONSE, 
                                       _, 
                                       _:16, 
                                       _, 
                                       _, 
                                       ?STATUS_NO_ERROR:16, 
                                       _:32, 
                                       _:32, 
                                       CAS2:64, 
                                       _/binary>>})->
                             if
                               Policy =:= lower_cas -> 
                                 CAS1 =< CAS2;
                               true ->
                                 CAS1 > CAS2
                             end
                         end, {}, Results1),
  Final;
do_action(Policy, Header, Key, [Node|Rest], [Last|Other] = All) ->
  case mixr_operation:is_error(Last) of
    {false, _} ->
      do_action(Policy, Header, Key, Rest, [rpc:call(Node, ?MODULE, action, [local, Header, Key])|All]);
    {true, _} ->
      do_action(Policy, Header, Key, Rest, [rpc:call(Node, ?MODULE, action, [local, Header, Key])|Other])
  end.
