% @hidden
-module(mixr_rest).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("../include/mixr.hrl").

-export([
         start_link/0
        ]).
-export([
         init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3
        ]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_) ->
  case doteki:get_as_atom([mixr, rest, enable], ?MIXR_DEFAULT_REST_ENABLE) of
    true ->
      _ = application:ensure_all_started(cowboy),
      Port = doteki:get_as_integer([mixr, rest, port], ?MIXR_DEFAULT_REST_PORT),
      Dispatch = cowboy_router:compile([{'_', [
                                               {"/d/:key/cas/:cas[/:extra]", mixr_rest_handler, []},
                                               {"/d/:key/expire/:expire", mixr_rest_handler, []},
                                               {"/d/:key[/:extra]", mixr_rest_handler, []},
                                               {"/count", mixr_rest_count_handler, []}
                                              ]
                                        }]),
      Options = case doteki:get_env([mixr, rest, ip], ?MIXR_DEFAULT_IP) of
                  undefined -> [{port, Port}];
                  IP -> [{port, Port}, {ip, bucinet:to_ip(IP)}]
                end,
      cowboy:start_http(http, 100,
                        Options,
                        [{env, [{dispatch, Dispatch}]}]);
    _ ->
      ignore
  end.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  _ = application:stop(cowboy),
  _ = application:stop(ranch),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
