-module(mixr).
-include("../include/mixr.hrl").

-export([start/0]).
-export([version/0]).

start() ->
  application:ensure_all_started(?MODULE).

version() ->
  case application:get_key(mixr, vsn) of
    {ok, Vsn} -> bucs:to_binary(Vsn);
    _ -> <<"undefined">>
  end.

