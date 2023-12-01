-module(input).

-export([
    groups_and_lines/1,
    lines/1,
    number_list/1,
    numbers/1,
    raw/1
]).

-spec groups_and_lines(Name :: string()) -> list(list(binary())).

groups_and_lines(Name) ->
    Groups = binary:split(raw(Name), [<<"\n\n">>], [global]),
    [[binary_to_list(Form) || Form <- binary:split(Group, [<<"\n">>], [global])] || Group <- Groups].

-spec raw(Name :: string()) -> Data :: binary().

raw(Name) ->
    Path = filename:join([code:priv_dir(aoc2023), Name ++ ".txt"]),
    case file:read_file(Path) of
        {ok, Data} ->
            Data;
        {error, enoent} ->
            {ok, Data} = fetch_day_input(Name),
            file:write_file(Path, Data),
            Data
    end.

-spec lines(Name :: string()) -> Lines :: list(binary()).

lines(Name) ->
    binary:split(raw(Name), [<<"\r\n">>, <<"\n">>], [global, trim]).

-spec number_list(Name :: string()) -> Numbers :: list(integer()).

number_list(Name) ->
    lists:map(fun binary_to_integer/1, lines(Name)).

numbers(Name) ->
    lists:map(fun binary_to_integer/1, binary:split(raw(Name), <<",">>, [global])).


fetch_day_input(Day) ->
    "day" ++ N = Day,
    {ok, {{_, 200, _}, _Headers, Data}} = httpc:request(
        get,
        {
            ["https://adventofcode.com/2023/day/", N, "/input"],
            [{"Cookie", session_cookie()}]
        },
        [{ssl, ssl_options()}],
        [{body_format, binary}]
    ),
    {ok, Data}.


session_cookie() ->
    {ok, Cookie} = file:read_file(filename:join(code:priv_dir(aoc2023), ".aoc-session.cookie")),
    Cookie.


ssl_options() ->
    [
        {verify, verify_peer},
        {cacerts, public_key:cacerts_get()},
        {customize_hostname_check, [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}
    ].
