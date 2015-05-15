-module(mixr_ets_store).

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
          file,
          tid
         }).

init(Args) ->
  File = elists:keyfind(file, 1, Args, filename:join([efile:user_home(),
                                                      ".data-" ++ eutils:to_string(node()) ++ ".mixr"])),
  Tid = elists:keyfind(tid, 1, Args, mixr),
  open(#s{file = File, tid = Tid}).

terminate(State) ->
  _ = save(State),
  ok.

count(#s{tid = Tid} = State) ->
  case ets:info(Tid) of
    undefined ->
      {0, State};
    InfoList ->
      {elists:keyfind(size, 1, InfoList, 0), State}
  end.

exist(#s{tid = Tid} = State, Key, CAS) ->
  case ets:lookup(Tid, Key) of
    [{Key, #r{cas = CAS1, expiration = 0}}] when CAS =:= 0; CAS1 =:= CAS ->
      {true, State};
    [{Key, #r{cas = CAS1, expiration = Expiration}}] when CAS =:= 0; CAS1 =:= CAS ->
      case edate:compare(Expiration, edate:today()) of
        -1 ->
          {true, State};
        _ ->
          {false, remove(State, Key)}
      end;
    _ ->
      {false, State}
  end.

save(#s{tid = Tid} = State, Key, Value, CAS, Expiration, Flags) ->
  Expiration1 = if
                  Expiration =:= 0 -> 0;
                  true -> edate:add(edate:today(), Expiration, seconds)
                end,
  Record = #r{
              key = Key,
              value = Value,
              cas = CAS,
              expiration = Expiration1,
              flag = Flags},
  Result = case lookup(State, Key) of
             {{ok, _}, State} ->
               _ = ets:update_element(Tid, Key, {2, Record}),
               {ok, State};
             {not_found, State1} ->
               _ = ets:insert(Tid, {Key, Record}),
               {ok, State1};
             _ ->
               {error, State}
           end,
  case Result of
    {ok, State2} ->
      {{ok, CAS}, save(State2)};
    {error, State2} ->
      {error, State2};
    _ ->
      {error, State}
  end.

find(State, Key) ->
  case lookup(State, Key) of
    {{ok,
      #r{key = Key, value = Value, cas = CAS, expiration = Expiration, flag = Flags}},
     State1} ->
      {{ok, {Key, Value, CAS, expiration(Expiration), Flags}}, State1};
    Other ->
      Other
  end.

delete(State, Key) ->
  case exist(State, Key, 0) of
    {true, State} ->
      {ok, remove(State, Key)};
    {false, State1} ->
      {not_found, State1}
  end.

append(State, Key, Value) ->
  xpend(State, Key, Value, fun(Current, New) ->
                               <<Current/binary, New/binary>>
                           end).

prepend(State, Key, Value) ->
  xpend(State, Key, Value, fun(Current, New) ->
                               <<New/binary, Current/binary>>
                           end).

% Private

lookup(#s{tid = Tid} = State, Key) ->
  case exist(State, Key, 0) of
    {true, State} ->
      case ets:lookup(Tid, Key) of
        [{Key, Data}] -> {{ok, Data}, State};
        _ -> {error, State}
      end;
    {false, State1} ->
      {not_found, State1}
  end.

remove(#s{tid = Tid} = State, Key) ->
  _ = ets:delete(Tid, Key),
  save(State).

expiration(0) -> 0;
expiration(Date) ->
  Expiration = calendar:datetime_to_gregorian_seconds(Date) -
               calendar:datetime_to_gregorian_seconds(edate:today()),
  if
    Expiration > 0 -> Expiration;
    true -> 1
  end.

xpend(#s{tid = Tid} = State, Key, Value, Fun) ->
  case lookup(State, Key) of
    {{ok,
      #r{key = Key, value = CurrentValue, cas = CAS, flag = 0} = Data},
     State1} ->
      _ = ets:update_element(Tid, Key, {2, Data#r{value = Fun(CurrentValue, Value)}}),
      {{ok, CAS}, save(State1)};
    {{ok,
      #r{cas = CAS}},
     State1} ->
      {{ok, CAS}, State1};
    Other ->
      Other
  end.

open(#s{file = File, tid = Tid} = State) ->
  case ets:file2tab(File) of
    {ok, Tid1} -> #s{file = File, tid = Tid1};
    {error, _} -> _ = ets:new(Tid, [set,
                                 named_table,
                                 {write_concurrency, true},
                                 {read_concurrency, true}]),
                  State
  end.

save(#s{file = File, tid = Tid} = State) ->
  _ = ets:tab2file(Tid, File),
  State.

