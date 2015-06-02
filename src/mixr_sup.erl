-module(mixr_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Shutdown), {I, {I, start_link, []}, permanent, Shutdown, Type, [I]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {
       {one_for_one, 5, 10},
       [
        ?CHILD(mixr_config, worker, 5000),
        ?CHILD(mixr_store, worker, 5000),
        ?CHILD(mixr_server_sup, supervisor, infinity),
        ?CHILD(mixr_rest, worker, 5000),
        ?CHILD(mixr_discover, worker, 5000),
        ?CHILD(mixr_plugins, worker, 5000)
       ]
      }
    }.

