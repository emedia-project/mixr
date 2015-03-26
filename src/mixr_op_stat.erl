-module(mixr_op_stat).
-include("../include/mixr.hrl"). 

-export([action/1]).

action(#request_header{magic = ?REQUEST,
                       opcode = ?OP_STAT,
                       opaque = Opaque}) ->
  {reply, lists:map(fun({Key, Value}) ->
                        mixr_operation:build_response(#response_header{
                                          opcode = ?OP_STAT,
                                          opaque = Opaque,
                                          key_length = size(Key),
                                          body_length = size(Key) + size(Value),
                                          extra_length = 0
                                         }, <<>>, Key, Value)
                    end, [{<<"pid">>, eutils:to_binary(os:getpid())}, 
                          {<<"version">>, os_version()},
                          {<<"time">>, os_time()},
                          {<<"keys">>, eutils:to_binary(mixr_store:count())},
                          {<<"storage">>, eutils:to_binary(mixr_store:module())},
                          {<<"search_policy">>, eutils:to_binary(mixr_config:search_policy())},
                          {<<"ip">>, mixr_config:server_ip()},
                          {<<"port">>, eutils:to_binary(mixr_config:port())},
                          {<<"servers">>, mixr_discover:servers_addrs()},
                          {<<>>, <<>>}])}. 

os_version() ->
  {_, OsName} = os:type(),
  case os:version() of
    {A, B, C} -> 
      eutils:to_binary(lists:flatten(io_lib:format("(~s) ~B.~B.~B", [OsName, A, B, C])));
    Other -> 
      eutils:to_binary(lists:flatten(io_lib:format("(~s) ~s", [OsName, Other])))
  end.

os_time() ->
  {Mega,Sec,Micro} = os:timestamp(),
  eutils:to_binary((Mega*1000000+Sec)*1000000+Micro).

