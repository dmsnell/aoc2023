-module(day1).
-behavior(aoc).

-export([input_type/0, p1/1, p2/1]).

input_type() -> lines.


p1(Lines) ->
    lists:sum(lists:map(fun calibration_number/1, Lines)).


next_number(_, <<>>) ->
        empty;
next_number(strings, <<_, Rest/binary>> = Input) ->
    case Input of
        <<A, _/binary>> when A >= $1 andalso A =< $9 -> [A, Rest];
        <<"one", _/binary>>   -> [$1, Rest];
        <<"two", _/binary>>   -> [$2, Rest];
        <<"three", _/binary>> -> [$3, Rest];
        <<"four", _/binary>>  -> [$4, Rest];
        <<"five", _/binary>>  -> [$5, Rest];
        <<"six", _/binary>>   -> [$6, Rest];
        <<"seven", _/binary>> -> [$7, Rest];
        <<"eight", _/binary>> -> [$8, Rest];
        <<"nine", _/binary>>  -> [$9, Rest];
        <<_, _/binary>>       -> next_number(strings, Rest)
    end;
next_number(digits, <<A, Rest/binary>>) when A >= $1 andalso A =< $9 ->
    [A, Rest];
next_number(Mode, <<_, Rest/binary>>) ->
    next_number(Mode, Rest).


calibration_number(Input) ->
    [Left, Rest] = next_number(digits, Input),
    calibration_number(Rest, Left, Left).

calibration_number(Input, Left, Right) ->
    case next_number(digits, Input) of
        [Next, Rest] -> calibration_number(Rest, Left, Next);
        empty        -> {N, <<>>} = string:to_integer(<<Left, Right>>), N
    end.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

example(p1) -> input:lines("day1_a");
example(p2) -> input:lines("day1_b").


p1_test() ->
    ?assertEqual(142, p1(example(p1))).

-endif.

p2(Lines) ->
    lists:sum(lists:map(fun calibration_strings/1, Lines)).

calibration_strings(Input) ->
    [Left, Rest] = next_number(strings, Input),
    calibration_strings(Rest, Left, nil).

calibration_strings(Input, Left, Right) ->
    case next_number(strings, Input) of
        [Next, Rest] -> calibration_strings(Rest, Left, Next);
        empty        -> case Right of
            nil -> {N, <<>>} = string:to_integer(<<Left, Left>>), N;
            _   -> {N, <<>>} = string:to_integer(<<Left, Right>>), N
        end
    end.

-ifdef(EUNIT).

p2_test() ->
    ?assertEqual(281, p2(example(p2))).

-endif.
