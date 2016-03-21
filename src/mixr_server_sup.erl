% @hidden
-module(mixr_server_sup).
-behaviour(supervisor).
-include("../include/mixr.hrl").

-export([start_link/0, start_child/1]).
-export([init/1]).

start_link() ->
  case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
    {ok, Pid} -> 
      start_child(Pid),
      {ok, Pid};
    Other ->
      Other
  end.

start_child(Server) ->
  supervisor:start_child(Server, []).

init([]) ->
  case doteki:get_as_atom([mixr, server, enable], ?MIXR_DEFAULT_SERVER_ENABLE) of
    true ->
      IP = doteki:get_env([mixr, server, ip], ?MIXR_DEFAULT_IP),
      Port = doteki:get_as_integer([mixr, server, port], ?MIXR_DEFAULT_SERVER_PORT),
      BasicSockOpts = [binary,
                       {packet, raw},
                       {active, false},
                       {reuseaddr, true}],
      SockOpts = case IP of
                   undefined -> BasicSockOpts;
                   _ -> [{ip, bucinet:to_ip(IP)} | BasicSockOpts]
                 end,
      {ok, LSock} = gen_tcp:listen(Port, SockOpts),
      {ok, {
         {simple_one_for_one, 5, 10},
         [
          {mixr_server, 
           {mixr_server, start_link, [mixr_dispatcher, LSock]},
           temporary, 
           2000, 
           worker, 
           [mixr_server]
          }
         ]
        }
      };
    false ->
      ignore
  end.

