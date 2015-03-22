-module(mixr_server).

-behaviour(gen_server).

-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).
-export([start_link/2]).

-record(state, {
          socket, 
          callback, 
          arg, 
          parent
         }).

start_link(Callback, Socket) ->
  gen_server:start_link(?MODULE, [Socket, Callback, [], self()], []).

init([Socket, Callback, Arg, Parent]) ->
  {ok, #state{socket = Socket, callback = Callback, arg = Arg, parent = Parent}, 0}.

handle_info({tcp, Socket, RawData}, #state{callback = Callback, arg = Arg} = State) ->
  case Callback:handle_data(Socket, RawData, Arg) of 
    {close, NewArg} ->
      gen_tcp:close(Socket),
      {noreply, State#state{arg = NewArg}};
    {ok, NewArg} -> 
      inet:setopts(Socket, [{active,once}]),
      {noreply, State#state{arg = NewArg}}
  end;

handle_info({tcp_closed, Socket}, #state{callback = Callback, arg = Arg} = State) ->
  Callback:handle_close(Socket, Arg),
  {stop, normal, State};

handle_info(timeout, #state{socket = Socket, callback = Callback, arg = Arg, parent = Parent} = State) ->
  {ok, Sock} = gen_tcp:accept(Socket),
  case Callback:handle_accept(Sock, Arg) of 
    {close, NewArg} -> 
      gen_tcp:close(Sock),
      {noreply, State#state{arg = NewArg}};
    {ok, NewArg} ->
      mixr_server_sup:start_child(Parent),
      inet:setopts(Sock, [{active,once}]),
      {noreply, State#state{socket = Sock, arg = NewArg}}
  end.

handle_call(Msg, _From, State) ->
  {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

