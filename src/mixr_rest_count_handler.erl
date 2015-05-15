-module(mixr_rest_count_handler).

-export([init/2]).

init(Req, Opts) ->
  Req2 = echo(Req),
  {ok, Req2, Opts}.

echo(Req) ->
  cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/plain; charset=utf-8">>}
  ], eutils:to_binary(mixr_store:count()), Req).
