-module(listlib).

-export([
    chunks/2
]).


chunks(N, List) when is_list(List) ->
    chunks(N, List, []).

chunks(N, [<<>>], Chunks) ->
    chunks(N, [], Chunks);
chunks(_N, [], Chunks) ->
    lists:reverse(Chunks);

chunks(N, List, Chunks) ->
    {Head, Tail} = lists:split(N, List),
    chunks(N, Tail, [Head | Chunks]).
