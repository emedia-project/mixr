-module(mixr_operation).
-include("../include/mixr.hrl"). 

-export([
         response/1, 
         response/4,
         build_response/4,
         response_or_quiet/1,
         response_or_quiet/4,
         error_response/2,
         error_response/3,
         error_response_or_quiet/3,
         noreply/0,
         is_error/1
        ]).


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

is_error({reply, <<?RESPONSE, _, _:16, _, _, ?STATUS_NO_ERROR:16, _/binary>>} = Response) ->
  {false, Response};
is_error(Response) ->
  {true, Response}.

