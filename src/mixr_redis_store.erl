-module(mixr_redis_store).
-behaviour(mixr_backend_store).

-export([
         init/1
         , terminate/1
         , count/1
         , exist/3
         , save/6
         , find/2
         , delete/2
         , append/3
         , prepend/3
        ]).

-record(r, {
          key,
          value,
          cas,
          expiration,
          flag}).

-record(s, {
          connection,
          namespace}).

init(Args) ->
  lager:info("Start redis store"),
  case eredis:start_link(
         elists:keyfind(host, 1, Args, "localhost"),
         elists:keyfind(port, 1, Args, 6379),
         elists:keyfind(database, 1, Args, 0),
         elists:keyfind(password, 1, Args, ""),
         elists:keyfind(reconnect_sleep, 1, Args, 100),
         elists:keyfind(timeout, 1, Args, 5000)) of
    {ok, C} ->
      #s{connection = C, namespace = elists:keyfind(namespace, 1, Args, "mixr")};
    _ ->
      lager:info("Redis connection faild!"),
      connection_faild
  end.

terminate(_) ->
  ok.

count(connection_faild) -> {0, connection_faild};
count(#s{connection = C} = State) ->
  COUNT = case eredis:q(C, ["KEYS", key(State, "*")]) of
            {ok, Data} -> length(Data);
            _ -> 0
          end,
  {COUNT, State}.

exist(connection_faild, _, _) -> {false, connection_faild};
exist(State, Key, CAS) ->
  case search(State, Key) of
    {Key, #r{cas = CAS1}} when CAS =:= 0; CAS1 =:= CAS ->
      {true, State};
    _ ->
      {false, State}
  end.

save(connection_faild, _, _, _, _, _) -> {error, connection_faild};
save(#s{connection = C} = State, Key, Value, CAS, Expiration, Flags) ->
  Record = #r{
              key = Key,
              value = Value,
              cas = CAS,
              expiration = Expiration,
              flag = Flags},
  case lookup(State, Key) of
    {{ok, _}, State} ->
      insert(State, Record);
    {not_found, State1} ->
      case insert(State1, Record) of
        {{ok, _}, _} = Result ->
          Exp = eutils:to_integer(Expiration),
          _ = if
                Exp > 0 ->
                  _ = eredis:q(C, ["EXPIRE", key(State1, Key), Exp]);
                true -> ok
              end,
          Result;
        _ ->
          {error, State1}
      end;
    _ ->
      {error, State}
  end.

find(connection_faild, _) -> {error, connection_faild};
find(State, Key) ->
  case lookup(State, Key) of
    {{ok,
      #r{key = Key, value = Value, cas = CAS, expiration = _, flag = Flags}},
     State1} ->
      {{ok, {Key, Value, CAS, expiration(State, Key), Flags}}, State1};
    Other ->
      Other
  end.

delete(connection_faild, _) -> {not_found, connection_faild};
delete(State, Key) ->
  case exist(State, Key, 0) of
    {true, State} ->
      _ = remove(State, Key),
      {ok, State};
    {false, State1} ->
      {not_found, State1}
  end.

append(connection_faild, _, _) -> {error, connection_faild};
append(State, Key, Value) ->
  xpend(State, Key, Value, fun(Current, New) ->
                               <<Current/binary, New/binary>>
                           end).

prepend(connection_faild, _, _) -> {error, connection_faild};
prepend(State, Key, Value) ->
  xpend(State, Key, Value, fun(Current, New) ->
                               <<New/binary, Current/binary>>
                           end).

% Private

field_num(Field) ->
  Fields = record_info(fields, r),
  DifField = fun (FieldName) -> Field /= FieldName end,
  case length(lists:takewhile(DifField, Fields)) of
    Length when Length =:= length(Fields) ->
      {error, not_found};
    Length ->
      Length + 2
  end.

set_field(Field, Value, Record) ->
  setelement(field_num(Field), Record, Value).

query_to_r({ok, Data}) ->
  query_to_r(Data, #r{});
query_to_r(Data) when is_list(Data) ->
  query_to_r(Data, #r{}).
query_to_r([], #r{cas = CAS, expiration = Exp, flag = Flag} = R) -> R#r{cas = eutils:to_integer(CAS),
                                                                        expiration = eutils:to_integer(Exp),
                                                                        flag = eutils:to_integer(Flag)};
query_to_r([Key, Value|Rest], R) ->
  query_to_r(Rest, set_field(eutils:to_atom(Key), Value, R)).

r_to_query(R) when is_record(R, r) ->
  lists:foldl(fun({K, V}, Acc) ->
                  [eutils:to_string(K), eutils:to_string(V) | Acc]
              end, [], lists:zip(record_info(fields, r), tl(tuple_to_list(R)))).

key(#s{namespace = NS}, Key) ->
  lists:flatten(eutils:to_string(NS) ++ ":" ++ eutils:to_string(Key)).

search(#s{connection = C} = State, Key) ->
  case eredis:q(C, ["HGETALL", key(State, Key)]) of
    {ok, Data} when Data =/= [] -> {Key, query_to_r(Data)};
    _ -> undefined
  end.

lookup(State, Key) ->
  case exist(State, Key, 0) of
    {true, State} ->
      case search(State, Key) of
        {Key, Data} -> {{ok, Data}, State};
        _ -> {error, State}
      end;
    {false, State1} ->
      {not_found, State1}
  end.

remove(#s{connection = C} = State, Key) ->
  case eredis:q(C, ["DEL", key(State, Key)]) of
    {ok, _} -> ok;
    E -> E
  end.

insert(#s{connection = C} = State, #r{key = Key, cas = CAS} = Record) ->
  case eredis:q(C, ["HMSET",
                    key(State, Key) | r_to_query(Record)]) of
    {ok, _} ->
      {{ok, CAS}, State};
    _ ->
      {error, State}
  end.

expiration(#s{connection = C} = State, Key) ->
  case eredis:q(C, ["TTL", key(State, Key)]) of
    {ok, Expiration} ->
      case eutils:to_integer(Expiration) > 0 of
        true -> Expiration;
        false -> "0"
      end;
    _ -> "0"
  end.

xpend(State, Key, Value, Fun) ->
  case lookup(State, Key) of
    {{ok,
      #r{key = Key, value = CurrentValue, cas = CAS, flag = 0} = Data},
     State1} ->
      _ = insert(State, Data#r{value = Fun(CurrentValue, Value)}),
      {{ok, CAS}, State1};
    {{ok,
      #r{cas = CAS}},
     State1} ->
      {{ok, CAS}, State1};
    Other ->
      Other
  end.

