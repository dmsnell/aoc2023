%%%-------------------------------------------------------------------
%% @doc aoc2023 public API
%% @end
%%%-------------------------------------------------------------------

-module(aoc2023_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    aoc2023_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
