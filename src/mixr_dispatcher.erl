-module(mixr_dispatcher).
-include("../include/mixr.hrl"). 

-export([handle_data/3, handle_accept/2, handle_close/2]).

handle_data(Sock, Data, State) ->
  {Header1, Body} = case Data of
                      <<Header:24/binary, Rest/binary>> -> {parse_request_header(Header), Rest};
                      <<Header:24/binary>> -> {parse_request_header(Header), <<>>};
                      _ -> {#request_header{}, <<>>}
                    end,
  lager:info("Request_header = ~p, body = ~p", [Header1, Body]),
  case action(Header1, Body) of
    {reply, Result} ->
      lists:foreach(fun(Response) ->
                        lager:info("Send response ~p", [Response]),
                        gen_tcp:send(Sock, Response)
                    end, if
                           is_list(Result) -> Result;
                           true -> [Result]
                         end);
    noreply ->
      lager:info("No reply")
  end,
  {ok, State}.

handle_accept(Sock, State) ->
  {ok, {IP, Port}} = inet:peername(Sock),
  lager:info("New client connected ~p:~p", [enet:ip_to_str(IP), Port]),
  {ok, State}.

handle_close(_Sock, _State) ->
  io:format("sock close~n").

parse_request_header(<<Magic, 
               Opcode, 
               KeyLength:16, 
               ExtraLength, 
               DataType, 
               VBucket:16, 
               TotalBody:32, 
               Opaque:32, 
               CAS:64>>) ->
  #request_header{
     magic = Magic,
     opcode = Opcode,
     key_length = KeyLength,
     extra_length = ExtraLength,
     data_type = DataType,
     vbucket = VBucket,
     body_length = TotalBody,
     opaque = Opaque,
     cas = CAS
    }.

%% Version
action(#request_header{magic = ?REQUEST, 
                       opcode = ?OP_VERSION} = Header, _) ->
  mixr_op_version:action(Header);

%% Set, Set Quietly, Add, Add Quietly, Replace, Replace Quietly
action(#request_header{magic = ?REQUEST, 
                       opcode = Opcode} = Header, 
       Body) when Opcode =:= ?OP_SET ;
                  Opcode =:= ?OP_ADD ;
                  Opcode =:= ?OP_REPLACE ;
                  Opcode =:= ?OP_SETQ ;
                  Opcode =:= ?OP_ADDQ ;
                  Opcode =:= ?OP_REPLACEQ -> 
  mixr_op_set:action(Header, Body);

%% Get, Get Quietly, Get Key, Get Key Quietly
action(#request_header{magic = ?REQUEST, 
                       opcode = Opcode} = Header, 
       Key) when Opcode =:= ?OP_GET ; 
                 Opcode =:= ?OP_GETQ ;
                 Opcode =:= ?OP_GETK ;
                 Opcode =:= ?OP_GETKQ ->
  mixr_op_get:action(mixr_config:search_policy(), Header, Key);

%% Delete
action(#request_header{magic = ?REQUEST, 
                       opcode = Opcode, 
                       extra_length = 0} = Header, Key) when Opcode =:= ?OP_DELETE ; 
                                                             Opcode =:= ?OP_DELETEQ ->
  mixr_op_delete:action(Header, Key);

%% Increment, Decrement

%% quit

%% Flush

%% Append, Prepend
action(#request_header{magic = ?REQUEST, 
                       opcode = Opcode} = Header, 
       Body) when Opcode =:= ?OP_APPEND ;
                  Opcode =:= ?OP_APPENDQ ;
                  Opcode =:= ?OP_PREPEND ;
                  Opcode =:= ?OP_PREPENDQ ->
  mixr_op_xpend:action(Header, Body);

%% noop

%% Stat
action(#request_header{magic = ?REQUEST,
                       opcode = ?OP_STAT} = Header, _) ->
  mixr_op_stat:action(Header);

%% Errors
action(#request_header{magic = ?REQUEST,
                       opcode = Opcode,
                       opaque = Opaque}, _) ->
  mixr_operation:error_response(Opcode, ?STATUS_NOT_SUPPORTED, Opaque);
action(_, _) ->
  mixr_operation:error_response(?OP_GET, ?STATUS_UNKNOWN_COMMAND).

