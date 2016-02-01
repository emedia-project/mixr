% @hidden
-module(mixr_backend_store).

-type state() :: any().
-type key() :: iolist().
-type value() :: iolist().
-type cas() :: integer().
-type expiration() :: integer().
-type flags() :: integer().

-callback init(any())
  -> state().

-callback terminate(state())
  -> ok
     | {error, term()}.

-callback count(state())
  -> {integer(), state()}.

-callback exist(state(), key(), cas())
  -> {true, state()}
     | {false, state()}.

-callback save(state(), key(), value(), cas(), expiration(), flags())
  -> {{ok, cas()}, state()}
     | {error, state()}.

-callback find(state(), key())
  -> {{ok, {key(), value(), cas(), expiration(), flags()}}, state()}
     | {not_found, state()}
     | {error, state()}.

-callback delete(state(), key())
  -> {ok, state()}
     | {not_found, state()}.

-callback append(state(), key(), value())
  -> {{ok, cas()}, state()}
     | {not_found, state()}
     | {error, state()}.

-callback prepend(state(), key(), value())
  -> {{ok, cas()}, state()}
     | {not_found, state()}
     | {error, state()}.

