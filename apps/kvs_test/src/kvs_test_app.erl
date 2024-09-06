%%%-------------------------------------------------------------------
%% @doc adder3 public API
%% @end
%%%-------------------------------------------------------------------

-module(kvs_test_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kvs_test_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
