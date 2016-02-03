% @hidden
-module(mixr_server_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

start_link() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  start_child(Pid),
  {ok, Pid}.

start_child(Server) ->
  supervisor:start_child(Server, []).

init([]) ->
  IP = mixr_config:ip(),
  Port = mixr_config:port(),
  BasicSockOpts = [binary,
                   {packet, raw},
                   {active, false},
                   {reuseaddr, true}],
  SockOpts = case IP of
               undefined -> BasicSockOpts;
               _ -> [{ip, IP} | BasicSockOpts]
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
  }.
