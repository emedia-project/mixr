-module(mixr_rest_handler).

-export([
         init/2
         , allowed_methods/2
         , content_types_provided/2
         , content_types_accepted/2
         , resource_exists/2
         , delete_resource/2
        ]).

-export([
         action/2
        ]).

-record(q, {
          key,
          cas,
          expire,
          extra
         }).

init(Req, Opts) ->
  random:seed(now()),
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"HEAD">>, <<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"text">>, <<"plain">>, '*'}, action}],
   Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"text">>, <<"plain">>, '*'}, action}],
   Req, State}.

resource_exists(Req, _State) ->
  {true, Req, #q{key = cowboy_req:binding(key, Req),
                 cas = case cowboy_req:binding(cas, Req) of
                         undefined -> 0;
                         CAS -> eutils:to_integer(CAS)
                       end,
                 expire = case cowboy_req:binding(expire, Req) of
                            undefined -> 0;
                            Expiration -> eutils:to_integer(Expiration)
                          end,
                 extra = case cowboy_req:binding(extra, Req) of
                           undefined -> value;
                           Extra -> eutils:to_atom(Extra)
                         end}}.

delete_resource(Req, #q{key = Key, cas = CAS} = State) ->
  case CAS =/= 0 andalso mixr_store:exist(Key, CAS) of
    true ->
      case mixr_store:delete(Key) of
        ok ->
          {true, Req, State};
        _ ->
          {stop, cowboy_req:reply(500, Req), State}
      end;
    false ->
      {stop, cowboy_req:reply(405, Req), State}
  end.

action(Req, #q{key = Key, cas = CAS, expire = Expire, extra = Extra} = State) ->
  Action = cowboy_req:method(Req),
  case Action of
    <<"GET">> ->
      case find(Key) of
        {ok, {Value, CAS1, Expire1}} ->
          if
            Extra =:= cas -> {eutils:to_binary(CAS1), Req, State};
            Extra =:= expire -> {eutils:to_binary(Expire1), Req, State};
            true -> {Value, Req, State}
          end;
        not_found ->
          {stop, cowboy_req:reply(404, Req), State}
      end;
    <<"HEAD">> ->
      case find(Key) of
        {ok, _} ->
          {<<"">>, Req, State};
        not_found ->
          {stop, cowboy_req:reply(404, Req), State}
      end;
    <<"POST">> ->
      case CAS =:= 0 andalso not mixr_store:exist(Key, CAS) of
        true ->
          store(Req, Key, Expire, CAS, State);
        false ->
          {stop, cowboy_req:reply(405, Req), State}
      end;
    <<"PUT">> ->
      case CAS =/= 0 andalso mixr_store:exist(Key, CAS) of
        true ->
          store(Req, Key, Expire, CAS, State);
        false ->
          {stop, cowboy_req:reply(405, Req), State}
      end
  end.

find(Key) ->
  Policy = mixr_config:search_policy(),
  case mixr_op_get:find(Policy, Key) of
    {_, {Key, Value, CAS, Expiration, _Flags}} = Data ->
      _ = mixr_op_get:save_if_needed(Policy, Data),
      {ok, {Value, CAS, Expiration}};
    _ ->
      not_found
  end.

body(Req) ->
  body(Req, <<>>).

body(Req, Data) ->
  case cowboy_req:body(Req) of
    {ok, New, Req2} ->
      {Req2, <<Data/binary, New/binary>>};
    {more, New, Req2} ->
      body(Req2, <<Data/binary, New/binary>>)
  end.

store(Req, Key, Expire, CAS, State) ->
  {Req2, Data} = body(Req),
  case mixr_store:save(Key, Data, mixr_utils:cas(CAS), Expire, 0) of
    {ok, CAS1} ->
      {true, cowboy_req:set_resp_body(eutils:to_binary(CAS1), Req2), State};
    _ ->
      {stop, cowboy_req:reply(500, Req2), State}
  end.
