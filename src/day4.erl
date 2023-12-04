-module(day4).
-behavior(aoc).

-export([input_type/0, p1/1, p2/1]).

input_type() -> raw.

p1(Input) ->
  lines(Input).

lines(Input) ->
  Cards = lines(Input, []),
  lists:sum([P || {_C, _M, P} <- Cards]).

lines(Lines, Points) ->
  case binary:split(Lines, <<"\n">>) of
    [Line, <<>>] ->
      Point = line(Line),
      lists:reverse([Point | Points]);

    [Line, Rest] ->
      Point = line(Line),
      lines(Rest, [Point | Points])
  end.

line(Input) ->
  line(prefix, Input, nil, [], 0).

line(prefix, <<"Card ", Input/binary>>, _Card, _Wins, _Matches) ->
  line(card_number, Input, _Card, _Wins, _Matches);

line(card_number, Input, _Card, _Wins, _Matches) ->
  {N, {At, Length}}     = day3:next_number(Input, 0),
  <<": ", Rest/binary>> = binary:part(Input, At + Length, size(Input) - At - Length),
  line(wins, Rest, N, [], _Matches);

line(wins, Input, Card, Wins, _Matches) ->
  {N, {At, Length}} = day3:next_number(Input, 0),
  case binary:part(Input, At + Length, size(Input) - At - Length) of
    <<" | ", Rest/binary>> ->
      line(mine, Rest, Card, [N | Wins], 0);
    <<" ", Rest/binary>>   ->
      line(wins, Rest, Card, [N | Wins], _Matches)
  end;

line(mine, Input, Card, Wins, Matches) ->
  case {day3:next_number(Input, 0), Matches} of
    {nil, 0} ->
      {Card, 0, 0};
    {nil, _}->
      {Card, Matches, floor(math:pow(2, Matches - 1))};
    {{N, {At, Length}}, _} ->
      Points = case lists:member(N, Wins) of true -> 1; false -> 0 end,
      Rest   = binary:part(Input, At + Length, size(Input) - At - Length),
      line(mine, Rest, Card, Wins, Matches + Points)
  end.


p2(Input) ->
  Lines = lines(Input, []),
  Cards = maps:from_list([{C, M} || {C, M, _P} <- Lines]),
  Stack = maps:keys(Cards),
  tally(Cards, Stack, 0).

tally(_Cards, [], Count) ->
  Count;

tally(Cards, [Card | Stack], Count) ->
  Matches = maps:get(Card, Cards),
  Copies  = lists:seq(Card + 1, Card + Matches),
  tally(Cards, Copies ++ Stack, Count + 1).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

example(p1) -> input:raw("day4_a").

p1_test() ->
  ?assertEqual(13, p1(example(p1))).

p2_test() ->
  ?assertEqual(30, p2(example(p1))).

-endif.
