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
      lager:info("Send response ~p", [Result]),
      gen_tcp:send(Sock, Result);
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
response(
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
    }, Extras, Keys, Value) ->
  {reply, <<Magic, 
            Opcode, 
            KeyLength:16, 
            ExtraLength, 
            DataType, 
            Status:16, 
            TotalBody:32, 
            Opaque:32, 
            CAS:64, 
            Extras/binary, 
            Keys/binary, 
            Value/binary>>}.

error_reponse(Opcode, Status) ->
  error_reponse(Opcode, Status, 0).
error_reponse(Opcode, Status, Opaque) ->
  response(
    #response_header{
       opcode = Opcode,
       status = Status,
       opaque = Opaque
      }, <<>>, <<>>, <<>>).

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

%% Set
action(#request_header{magic = ?REQUEST, 
                       opcode = Opcode, 
                       key_length = KeyLength, 
                       extra_length = ExtraLength, 
                       body_length = BodyLength, 
                       opaque = Opaque,
                       cas = CAS}, 
       <<Flags:32, Expiration:32, Body/binary>> = Body1) when Opcode =:= ?OP_SET -> 
  if
    size(Body1) =:= BodyLength, ExtraLength =:= 8 ->
      ValueSize = BodyLength - ExtraLength - KeyLength,
      <<Key:KeyLength/binary, Value:ValueSize/binary>> = Body,
      if
        CAS =:= 0 -> lager:info("No CAS faild if key exist!"); %% TODO
        true -> lager:info("CAS, faild if key have a different CAS") %% TODO
      end,
      NewCAS = cas(CAS),
      lager:info("Store ~p = ~p / CAS = ~p / Expiration = ~p / Flags = ~p", [Key, Value, NewCAS, Expiration, Flags]),
      case mixr_store:save(Key, Value, NewCAS, Expiration, Flags) of
        error ->
          error_reponse(Opcode, ?STATUS_INTERNAL_ERROR, Opaque);
        {ok, NewCAS} ->
          response(
            #response_header{opcode = ?OP_SET,
                             opaque = Opaque,
                             cas = NewCAS})
      end;
    true ->
      error_reponse(Opcode, ?STATUS_INVALID_ARGUMENT, Opaque)
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
    size(Key) =:= BodyLength, KeyLength =:= BodyLength ->
      lager:info("Get value for Key = ~p", [Key]),
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
          error_reponse(Opcode, ?STATUS_KEY_NOT_FOUND, Opaque);
        E ->
          lager:info("Invalid find response: ~p", [E]),
          error_reponse(Opaque, ?STATUS_INTERNAL_ERROR, Opaque)
      end;
    true ->
      if
        Opcode =:= ?OP_GETQ ; Opcode =:= ?OP_GETKQ ->
          noreply();
        true ->
          error_reponse(Opcode, ?STATUS_INVALID_ARGUMENT, Opaque)
      end
  end;

%% Errors
action(#request_header{magic = ?REQUEST,
                       opcode = Opcode,
                       opaque = Opaque}, _) ->
  error_reponse(Opcode, ?STATUS_INVALID_ARGUMENT, Opaque);
action(_, _) ->
  error_reponse(?OP_GET, ?STATUS_UNKNOWN_COMMAND).

%% CAS
cas(0) ->
  {Mega,Sec,Micro} = erlang:now(),
  (Mega*1000000+Sec)*1000000+Micro;
cas(CAS) -> CAS.


