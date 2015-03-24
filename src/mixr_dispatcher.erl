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

response(Header) ->
  response(Header, <<>>, <<>>, <<>>).
response(Header, Extras, Key, Value) ->
  {reply, build_response(Header, Extras, Key, Value)}.

build_response(
  #response_header{
     magic = Magic,
     opcode = Opcode,
     key_length = KeyLength,
     extra_length = ExtraLength,
     data_type = DataType,
     status = Status,
     body_length = TotalBody,
     opaque = Opaque,
     cas = CAS
    }, Extras, Key, Value) ->
  <<Magic, 
    Opcode, 
    KeyLength:16, 
    ExtraLength, 
    DataType, 
    Status:16, 
    TotalBody:32, 
    Opaque:32, 
    CAS:64, 
    Extras/binary, 
    Key/binary, 
    Value/binary>>.

response_or_quiet(Header) ->
  response_or_quiet(Header, <<>>, <<>>, <<>>).
response_or_quiet(#response_header{opcode = Opcode} = Header, Extras, Key, Value) ->
  if
    ?QUIET(Opcode) ->
      noreply();
    true ->
      response(Header, Extras, Key, Value)
  end.

error_response(Opcode, Status) ->
  error_response(Opcode, Status, 0).
error_response(Opcode, Status, Opaque) ->
  response(
    #response_header{
       opcode = Opcode,
       status = Status,
       opaque = Opaque
      }, <<>>, <<>>, <<>>).

error_response_or_quiet(Opcode, Status, Opaque) ->
  if
    ?QUIET(Opcode) ->
      noreply();
    true ->
      error_response(Opcode, Status, Opaque)
  end.

noreply() -> noreply.

%% Version
action(#request_header{magic = ?REQUEST, 
                       opcode = ?OP_VERSION, 
                       opaque = Opaque}, <<>>) ->
  lager:info("request version"),
  response(
    #response_header{opcode = ?OP_VERSION, 
                     opaque = Opaque, 
                     body_length = size(?MIXR_VERSION)},
    <<>>,
    <<>>,
    ?MIXR_VERSION);

%% Set, Set Quietly, Add, Add Quietly, Replace, Replace Quietly
action(#request_header{magic = ?REQUEST, 
                       opcode = Opcode, 
                       key_length = KeyLength, 
                       extra_length = ExtraLength, 
                       body_length = BodyLength, 
                       opaque = Opaque,
                       cas = CAS}, 
       <<Flags:32, Expiration:32, Body/binary>> = Body1) when Opcode =:= ?OP_SET ;
                                                              Opcode =:= ?OP_ADD ;
                                                              Opcode =:= ?OP_REPLACE ;
                                                              Opcode =:= ?OP_SETQ ;
                                                              Opcode =:= ?OP_ADDQ ;
                                                              Opcode =:= ?OP_REPLACEQ -> 
  if
    size(Body1) =:= BodyLength andalso ExtraLength =:= 8 ->
      ValueSize = BodyLength - ExtraLength - KeyLength,
      <<Key:KeyLength/binary, Value:ValueSize/binary>> = Body,
      lager:info("[SET/~p] ~p = ~p", [Opcode, Key, Value]),
      case set_data(mixr_store:exist(Key, CAS), Opcode) of
        false -> 
          error_response(Opcode, ?STATUS_KEY_EXISTS, Opaque);
        true -> 
          NewCAS = cas(CAS),
          case mixr_store:save(Key, Value, NewCAS, Expiration, Flags) of
            error ->
              error_response(Opcode, ?STATUS_INTERNAL_ERROR, Opaque);
            {ok, NewCAS} ->
              response_or_quiet(
                #response_header{opcode = Opcode,
                                 opaque = Opaque,
                                 cas = NewCAS})
          end
      end;
    true ->
      error_response(Opcode, ?STATUS_INVALID_ARGUMENT, Opaque)
  end;

%% Get, Get Quietly, Get Key, Get Key Quietly
action(#request_header{magic = ?REQUEST, 
                       opcode = Opcode, 
                       key_length = KeyLength, 
                       extra_length = 0, 
                       body_length = BodyLength, 
                       opaque = Opaque,
                       cas = 0}, Key) when Opcode =:= ?OP_GET ; 
                                           Opcode =:= ?OP_GETQ ;
                                           Opcode =:= ?OP_GETK ;
                                           Opcode =:= ?OP_GETKQ ->
  if 
    size(Key) =:= BodyLength andalso KeyLength =:= BodyLength ->
      lager:info("[GET/~p] ~p", [Opcode, Key]),
      case mixr_store:find(Key) of
        {ok, {Key, Value, CAS, _, Flags}} ->
          Key1 = if
                   Opcode =:= ?OP_GETK ; Opcode =:= ?OP_GETKQ ->
                     Key;
                   true ->
                     <<>>
                 end,
          Flags1 = <<Flags:32>>,
          response(
            #response_header{
               opcode = ?OP_GET,
               extra_length = size(Flags1),
               body_length = size(Key1) + size(Value) + size(Flags1),
               opaque = Opaque,
               cas = CAS}, Flags1, Key1, Value);
        not_found ->
          error_response_or_quiet(Opcode, ?STATUS_KEY_NOT_FOUND, Opaque);
        _ ->
          error_response(Opaque, ?STATUS_INTERNAL_ERROR, Opaque)
      end;
    true ->
      error_response(Opcode, ?STATUS_INVALID_ARGUMENT, Opaque)
  end;

%% Delete
action(#request_header{magic = ?REQUEST, 
                       opcode = Opcode, 
                       key_length = KeyLength, 
                       extra_length = 0, 
                       body_length = BodyLength, 
                       opaque = Opaque}, Key) when Opcode =:= ?OP_DELETE ; 
                                                   Opcode =:= ?OP_DELETEQ ->
  if 
    size(Key) =:= BodyLength andalso KeyLength =:= BodyLength ->
      lager:info("[DELETE/~p] ~p", [Opcode, Key]),
      case mixr_store:delete(Key) of
        ok -> 
          response_or_quiet(
            #response_header{opcode = Opcode,
                             opaque = Opaque});
        not_found ->
          error_response_or_quiet(Opcode, ?STATUS_KEY_NOT_FOUND, Opaque)
      end;
    true ->
      error_response(Opcode, ?STATUS_INVALID_ARGUMENT, Opaque)
  end;

%% Increment, Decrement

%% quit

%% Flush

%% Append, Prepend
action(#request_header{magic = ?REQUEST, 
                       opcode = Opcode, 
                       key_length = KeyLength, 
                       extra_length = ExtraLength, 
                       body_length = BodyLength, 
                       opaque = Opaque}, 
       Body) when Opcode =:= ?OP_APPEND ;
                  Opcode =:= ?OP_APPENDQ ;
                  Opcode =:= ?OP_PREPEND ;
                  Opcode =:= ?OP_PREPENDQ ->
  if
    size(Body) =:= BodyLength andalso ExtraLength =:= 0 ->
      ValueSize = BodyLength - KeyLength,
      <<Key:KeyLength/binary, Value:ValueSize/binary>> = Body,
      lager:info("[APPEND|PREPEND/~p] ~p :: ~p", [Opcode, Key, Value]),
      Result = if 
                 Opcode =:= ?OP_APPEND orelse Opcode =:= ?OP_APPENDQ ->
                   mixr_store:append(Key, Value);
                 true ->
                   mixr_store:prepend(Key, Value)
               end,
      case Result of
        {ok, CAS} ->
          response_or_quiet(
            #response_header{opcode = Opcode,
                             opaque = Opaque,
                             cas = CAS});
        not_found ->
          error_response(Opcode, ?STATUS_KEY_NOT_FOUND, Opaque);
        error ->
          error_response(Opaque, ?STATUS_INTERNAL_ERROR, Opaque)
      end;
    true ->
      error_response(Opcode, ?STATUS_INVALID_ARGUMENT, Opaque)
  end;

%% noop

%% Stat
action(#request_header{magic = ?REQUEST,
                       opcode = ?OP_STAT,
                       opaque = Opaque}, _) ->
  {reply, lists:map(fun({Key, Value}) ->
                        build_response(#response_header{
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
                          {<<>>, <<>>}])}; 

%% Errors
action(#request_header{magic = ?REQUEST,
                       opcode = Opcode,
                       opaque = Opaque}, _) ->
  error_response(Opcode, ?STATUS_NOT_SUPPORTED, Opaque);
action(_, _) ->
  error_response(?OP_GET, ?STATUS_UNKNOWN_COMMAND).

%% CAS
cas(0) ->
  {Mega,Sec,Micro} = erlang:now(),
  (Mega*1000000+Sec)*1000000+Micro;
cas(CAS) -> CAS.

set_data(_, ?OP_SET) -> true;
set_data(true, ?OP_REPLACE) -> true;
set_data(false, ?OP_ADD) -> true;
set_data(_, ?OP_SETQ) -> true;
set_data(true, ?OP_REPLACEQ) -> true;
set_data(false, ?OP_ADDQ) -> true;
set_data(_, _) -> false.

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

