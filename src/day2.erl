-module(day2).
-behavior(aoc).

-export([input_type/0, parse_input/1, p1/1]).

input_type() -> lines.

parse_input(Lines) ->
  maps:from_list([parse(prefix, Line, {nil, []}, nil) || Line <- Lines]).

parse(prefix, <<"Game ", Rest/binary>>, Game, Round) ->
  parse(game_id, Rest, Game, Round);

parse(game_id, Line, {nil, Rounds}, _Round) ->
  {GameId, <<": ", Rest/binary>>} = digits(Line),
  parse(count, Rest, {GameId, Rounds}, #{red => 0, green => 0, blue => 0});

parse(count, Line, Game, Round) ->
  {Count, <<" ", Rest/binary>>} = digits(Line),
  parse({color, Count}, Rest, Game, Round);

parse({color, Count}, <<"red", Rest/binary>>, Game, Round) ->
  parse(after_color, Rest, Game, Round#{red => Count});
parse({color, Count}, <<"green", Rest/binary>>, Game, Round) ->
  parse(after_color, Rest, Game, Round#{green => Count});
parse({color, Count}, <<"blue", Rest/binary>>, Game, Round) ->
  parse(after_color, Rest, Game, Round#{blue => Count});

parse(after_color, <<", ", Rest/binary>>, Game, Round) ->
  parse(count, Rest, Game, Round);
parse(after_color, <<"; ", Rest/binary>>, {GameId, Rounds}, Round) ->
  parse(count, Rest, {GameId, [Round | Rounds]}, #{red => 0, green => 0, blue => 0});
parse(after_color, <<>>, {GameId, Rounds}, Round) ->
  {GameId, lists:reverse([Round | Rounds])}.

p1(Games) ->
  {Rmax, Gmax, Bmax} = {12, 13, 14},
  Possible = maps:filter(
    fun (_GameId, Rounds) ->
        lists:all(
          fun (#{red := R, green := G, blue := B}) ->
            R =< Rmax andalso G =< Gmax andalso B =< Bmax
          end,
          Rounds
        )
    end,
    Games
  ),
  lists:sum(maps:keys(Possible)).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

example(p1) -> parse_input(input:lines("day2_a")).

p1_test() ->
  ?assertEqual(8, p1(example(p1))).


-endif.


digits(S) ->
  digits(S, []).

digits(<<A, S/binary>>, Digits) when A >= $0 andalso A =< $9 ->
  digits(S, [A | Digits]);
digits(S, [_|_] = Digits) ->
  {N, []} = string:to_integer(lists:reverse(Digits)),
  {N, S}.
