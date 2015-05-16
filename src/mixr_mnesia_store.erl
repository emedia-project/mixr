-module(mixr_mnesia_store).
-include_lib("stdlib/include/qlc.hrl").

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
          path
         }).

init(Args) ->
  Path = elists:keyfind(path, 1, Args, filename:join([efile:user_home(),
                                                      ".mixr-" ++ eutils:to_string(node())])),
  open(#s{path = Path}).

terminate(_State) ->
  ok.

count(State) ->
  try
    {mnesia:table_info(r, size), State}
  catch
    _:_ ->
      {0, State}
  end.

exist(State, Key, CAS) ->
  case search(Key) of
    {ok, #r{cas = CAS1, expiration = 0}} when CAS =:= 0; CAS1 =:= CAS ->
      {true, State};
    {ok, #r{cas = CAS1, expiration = Expiration} = Data} when CAS =:= 0; CAS1 =:= CAS ->
      case edate:compare(Expiration, edate:today()) of
        -1 ->
          {true, State};
        _ ->
          _ = remove(Data),
          {false, State}
      end;
    _ ->
      {false, State}
  end.

save(State, Key, Value, CAS, Expiration, Flags) ->
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
  _ = mnesia:transaction(fun() ->
                             mnesia:write(Record)
                         end),
  {{ok, CAS}, State}.

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
  case lookup(State, Key) of
    {{ok, Data}, State1} ->
      _ = remove(Data),
      {ok, State1};
    {_, State2} ->
      {not_found, State2}
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

lookup(State, Key) ->
  case exist(State, Key, 0) of
    {true, State} ->
      case search(Key) of
        {ok, Data} -> {{ok, Data}, State};
        _ -> {error, State}
      end;
    {false, State1} ->
      {not_found, State1}
  end.

remove(Data) ->
  _ = mnesia:transaction(fun() ->
                             mnesia:delete_object(Data)
                         end).

expiration(0) -> 0;
expiration(Date) ->
  Expiration = calendar:datetime_to_gregorian_seconds(Date) -
               calendar:datetime_to_gregorian_seconds(edate:today()),
  if
    Expiration > 0 -> Expiration;
    true -> 1
  end.

xpend(State, Key, Value, Fun) ->
  case lookup(State, Key) of
    {{ok,
      #r{key = Key, value = CurrentValue, cas = CAS, flag = 0} = Data},
     State1} ->
      _ = mnesia:transaction(fun() ->
                                 mnesia:write(Data#r{value = Fun(CurrentValue, Value)})
                             end),
      {{ok, CAS}, State1};
    {{ok,
      #r{cas = CAS}},
     State1} ->
      {{ok, CAS}, State1};
    Other ->
      Other
  end.

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

uniq(Data) ->
  case length(Data) of
    0 -> undefined;
    1 -> [Result|_] = Data, {ok, Result};
    _ -> error
  end.

search(Key) ->
  uniq(do(qlc:q([X || X <- mnesia:table(r),
                      X#r.key =:= Key
                ]))).

open(#s{path = Path} = State) ->
  application:set_env(mnesia, dir, Path),
  Schema = mnesia:create_schema([node()]),
  mnesia:start(),
  case Schema of
    ok ->
      lager:info("Create schema."),
      X = mnesia:create_table(r,
                          [{disc_copies, [node()]}, {attributes, record_info(fields, r)}]),
      lager:info("==> ~p", [X]),
      created;
    {error,{_,{already_exists,_}}} ->
      lager:info("Schema already exist."),
      complete;
    {error, Reason} ->
      lager:error("Error : ~p", [Reason]),
      throw(Reason)
  end,
  case mnesia:wait_for_tables([r], 20000) of
    {timeout, RemainingTabs} ->
      throw(RemainingTabs);
    ok ->
      State
  end.

