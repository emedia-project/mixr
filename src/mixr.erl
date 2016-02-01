% @hidden
-module(mixr).

-export([start/0]).

start() ->
  application:ensure_all_started(?MODULE).

