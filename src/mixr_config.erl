-module(mixr_config).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/0
         , ip/0
         , port/0
         , auto_discover/0
         , store/0
         , search_policy/0
         , rest/0
         , plugins/0
         , server_ip/0
         , version/0
        ]).

-define(ACCESSOR(Type), Type() -> gen_server:call(?MODULE, Type)).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

?ACCESSOR(ip).
?ACCESSOR(port).
?ACCESSOR(auto_discover).
?ACCESSOR(store).
?ACCESSOR(search_policy).
?ACCESSOR(rest).
?ACCESSOR(plugins).

server_ip() ->
  case ip() of
    IP when is_tuple(IP), IP =/= {0,0,0,0} -> eutils:to_binary(enet:ip_to_str(IP));
    _ -> eutils:to_binary(enet:ip_to_str(enet:get_active_ip()))
  end.

version() ->
  case application:get_key(mixr, vsn) of
    {ok, Vsn} -> eutils:to_binary(Vsn);
    _ -> <<"undefined">>
  end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  try
    {ok, lists:foldl(fun({Key, Value}, Acc) ->
                         maps:put(Key, Value, Acc)
                     end,
                     #{ip => undefined,
                       port => 11212,
                       auto_discover => [{enable, false}],
                       search_policy => local,
                       store => mixr_mem_store,
                       rest => [{enable, false}]},
                     application:get_all_env(mixr))}
  catch
    error:E -> {stop, E}
  end.

handle_call(Key, _From, State) ->
  try
    {reply, maps:get(Key, State), State}
  catch
    _:_ -> {reply, {error, missing_key}, State}
  end.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

