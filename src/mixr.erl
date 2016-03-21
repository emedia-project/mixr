-module(mixr).

-export([start/0]).
-export([
         version/0
         , set/2
         , get/1
         , delete/1
        ]).

start() ->
  application:ensure_all_started(?MODULE).

version() ->
  case application:get_key(mixr, vsn) of
    {ok, Vsn} -> bucs:to_binary(Vsn);
    _ -> <<"undefined">>
  end.

%version() ->
%  ok.

set(_Key, _Value) ->
  ok.

get(_Key) ->
  ok.

delete(_Key) ->
  ok.
