% @hidden
-module(mixr_plugins).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-include("../include/mixr.hrl").
-define(SERVER, ?MODULE).

-export([
         start_link/0,
         list/0,
         find/2,
         find/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

list() ->
  gen_server:call(?MODULE, list).

find(Plugin, Key) ->
  gen_server:call(?MODULE, {find, Plugin, Key}).

find(Key) ->
  gen_server:call(?MODULE, {find, Key}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
  {ok, lists:foldl(fun(Plugin, Acc) ->
                       {Name, Options} = case Plugin of
                                           {N, O} -> {bucs:to_atom(N), O};
                                           N -> {bucs:to_atom(N), []}
                                         end,
                       case erlang:apply(Name, start, [Options]) of
                         {ok, State1} ->
                           lager:info("Start plugin ~p", [Name]),
                           maps:put(Name, State1, Acc);
                         error ->
                           lager:info("Faild to start plugin ~p", [Name]),
                           Acc
                       end
                   end, #{}, doteki:get_env([mixr, plugins], ?MIXR_DEFAULT_PLUGINS))}.

handle_call({find, Plugin, Key}, _From, State) ->
  case maps:get(bucs:to_atom(Plugin), State, '**undefined**') of
    '**undefined**' ->
      {reply, not_found, State};
    Options ->
      {reply, call(Plugin, Key, Options), State}
  end;
handle_call({find, Key}, _From, State) ->
  {reply, call(Key, maps:to_list(State)), State};
handle_call(list, _From, State) ->
  {reply, maps:keys(State), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  _ = lists:foreach(fun({Plugin, Options}) ->
                        lager:info("Stop plugin ~p: ~p", [Plugin,
                                                          erlang:apply(Plugin, stop, [Options])])
                    end, maps:to_list(State)),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

call(_, []) ->
  not_found;
call(Key, [{Plugin, Options}|Rest]) ->
  case call(Plugin, Key, Options) of
    {ok, _} = Result ->
      Result;
    _ ->
      call(Key, Rest)
  end.

call(Plugin, Key, Options) ->
  lager:info("Call ~p for key ~s", [Plugin, Key]),
  case erlang:apply(bucs:to_atom(Plugin), find, [Key, Options]) of
    {ok, {Key1, Value1, CAS, Expire, Flags}} ->
      {ok, {Key1, Value1, mixr_utils:cas(CAS), Expire, Flags}};
    _ ->
      not_found
  end.
