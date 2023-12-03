-module(day3).
-behavior(aoc).

-export([input_type/0, p1/1, p2/1]).

input_type() -> raw.

%% 467..114..
%% ...*......
%% ..35..633.
%% ......#...
%% 617*......
%% .....+.58.
%% ..592.....
%% ......755.
%% ...$.*....
%% .664.598..

p1(Input) ->
  {FirstLine, AfterFirst} = next_line(Input),
  Cols                    = size(FirstLine),
  PaddingLine             = <<<<".">> || _ <- lists:seq(1, Cols)>>,
  {SecondLine, Rest}      = next_line(AfterFirst),
  line({PaddingLine, FirstLine, SecondLine}, <<Rest/binary, PaddingLine/binary>>, 0).


line({_, This, _} = Region, <<>>, Sum) ->
  line_number(Region, next_number(This, 0), Sum);
line({_, This, Next} = Region, Lines, Sum) ->
  LineSum          = line_number(Region, next_number(This, 0), 0),
  {UberNext, Rest} = next_line(Lines),
  line({This, Next, UberNext}, Rest, Sum + LineSum).

line_number(_Region, nil, Sum) ->
  Sum;
line_number({_, This, _} = Region, {N, {At, Length}}, Sum) ->
  Addition = case touches(Region, {At, Length}) of
    true  -> N;
    false -> 0
  end,
  line_number(Region, next_number(This, At + Length), Sum + Addition).

touches(Region, {At, Length}) ->
  touches(above, Region, {At, Length}).

touches(above, {Prev, _, _} = Region, {At, Length}) ->
  case has_symbol(binary:part(Prev, At - 1, Length + 2)) of
    true  -> true;
    false -> touches(before, Region, {At, Length})
  end;

touches(before, {_, This, _} = Region, {At, Length}) ->
  case is_symbol(binary:at(This, At - 1)) of
    true  -> true;
    false -> touches(behind, Region, {At, Length})
  end;

touches(behind, {_, This, _} = Region, {At, Length}) ->
  case is_symbol(binary:at(This, At + Length)) of
    true  -> true;
    false -> touches(below, Region, {At, Length})
  end;

touches(below, {_, _, Next}, {At, Length}) ->
  has_symbol(binary:part(Next, At - 1, Length + 2)).


has_symbol(<<>>) -> false;
has_symbol(<<A, Rest/binary>>) ->
  case is_symbol(A) of
    true  -> true;
    false -> has_symbol(Rest)
  end.

is_symbol(A) -> A =/= $. andalso (A < $0 orelse A > $9).

next_line(Input) ->
  case binary:split(Input, <<"\n">>) of
    [Line]           -> {pad(Line), <<>>};
    [Line, <<"\n">>] -> {pad(Line), <<>>};
    [Line, Rest]     -> {pad(Line), Rest}
  end.

pad(Line) -> <<".", Line/binary, ".">>.

next_number(Input, At) ->
  next_number(binary:part(Input, At, size(Input) - At), At, nil, []).

next_number(<<>>, _At, nil, _Digits) ->
  nil;
next_number(<<>>, _At, {Start, Length}, Digits) ->
  {N, []} = string:to_integer(lists:reverse(Digits)),
  {N, {Start, Length}};
next_number(<<A, Rest/binary>>, At, nil, _Digits) when A >= $0 andalso A =< $9 ->
  next_number(Rest, At + 1, {At, 1}, [A]);
next_number(<<A, Rest/binary>>, At, {Start, Length}, Digits) when A >= $0 andalso A =< $9 ->
  next_number(Rest, At + 1, {Start, Length + 1}, [A | Digits]);
next_number(<<_, Rest/binary>>, At, nil, Digits) ->
  next_number(Rest, At + 1, nil, Digits);
next_number(<<_, _Rest/binary>>, _At, {Start, Length}, Digits) ->
  {N, []} = string:to_integer(lists:reverse(Digits)),
  {N, {Start, Length}}.

p2(Input) ->
  {FirstLine, AfterFirst} = next_line(Input),
  Cols                    = size(FirstLine),
  PaddingLine             = <<<<".">> || _ <- lists:seq(1, Cols)>>,
  {SecondLine, Rest}      = next_line(AfterFirst),
  Above = [],
  Inline = all_numbers(FirstLine),
  Below = all_numbers(SecondLine),
  gears(
    {FirstLine, SecondLine},
    {Above, Inline, Below},
    <<Rest/binary, PaddingLine/binary>>,
    0
  ).


gears({This, _}, {Above, Inline, Below}, <<>>, Sum) ->
  case all_gears(This) of
    [] ->
      Sum;

    Gears ->
      LineSum          = gear_ratios(Gears, [Above, Inline, Below]),
      Sum + LineSum
  end;
gears({This, Next}, {Above, Inline, Below}, Lines, Sum) ->
  case all_gears(This) of
    [] ->
      {UberNext, Rest} = next_line(Lines),
      UberBelow        = all_numbers(UberNext),
      gears({Next, UberNext}, {Inline, Below, UberBelow}, Rest, Sum);

    Gears ->
      LineSum          = gear_ratios(Gears, [Above, Inline, Below]),
      {UberNext, Rest} = next_line(Lines),
      UberBelow        = all_numbers(UberNext),
      gears({Next, UberNext}, {Inline, Below, UberBelow}, Rest, Sum + LineSum)
  end.

gear_ratios(Gears, Numbers) ->
  lists:sum(
    [Ratio || Ratio <- [gear_ratio(Gear, Numbers) || Gear <- Gears], Ratio =/= false]
  ).

gear_ratio(Gear, Numbers) ->
  gear_ratio(Gear, Numbers, []).

gear_ratio(_Gear, [], [A, B]) ->
  A * B;
gear_ratio(_Gear, [], _Matches) ->
  false;
gear_ratio(Gear, [Before | Numbers], Matches) ->
  case gear_matches(Gear, Before, Matches) of
    false ->
      false;
    LineMatches ->
      gear_ratio(Gear, Numbers, LineMatches)
  end.

gear_matches(_Gear, _Numbers, [_, _, _]) ->
  false;
gear_matches(_Gear, [], [A, B]) ->
  [A, B];
gear_matches(_Gear, [], Matches) ->
  Matches;
gear_matches(Gear, [{_, {At, _}} | _], Matches) when Gear < At - 1 ->
  Matches;
gear_matches(Gear, [{N, {At, Length}} | Numbers], Matches) ->
  Overlaps = (
      (At - 1 =< Gear andalso At + Length >= Gear) orelse
      At == Gear orelse
      At == Gear + 1
  ),
  case Overlaps of
    true  -> gear_matches(Gear, Numbers, [N | Matches]);
    false -> gear_matches(Gear, Numbers, Matches)
  end.


all_gears(Line) ->
  all_gears(Line, 0, []).

all_gears(<<>>, _At, Gears) ->
  lists:reverse(Gears);
all_gears(<<"*", Line/binary>>, At, Gears) ->
  all_gears(Line, At + 1, [At | Gears]);
all_gears(<<_, Line/binary>>, At, Gears) ->
  all_gears(Line, At + 1, Gears).


all_numbers(Line) ->
  all_numbers(Line, next_number(Line, 0), []).

all_numbers(_, nil, Numbers) ->
  lists:reverse(Numbers);
all_numbers(Line, {N, {At, Length}}, Numbers) ->
  all_numbers(Line, next_number(Line, At + Length), [{N, {At, Length}} | Numbers]).


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

example(p1) -> input:raw("day3_a").

p1_test() ->
  ?assertEqual(4361, p1(example(p1))).

p2_test() ->
  ?assertEqual(467835, p2(example(p1))).

-endif.
