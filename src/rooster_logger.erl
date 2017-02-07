-module(rooster_logger).

-export([info/1, err/1]).


info(El) ->
    print_elements(El, "INFO").

err(El) ->
    print_elements(El, "ERROR").

create_header(Type) ->
    {{Year, Month, Day}, {Hour, Min, Second}} = calendar:local_time(),
    FormattedDate = lists:concat([Month, "-", Day, "-", Year, "::", Hour, ":", Min, ":", Second]),
    lists:concat(["~n= ", Type, " === ", FormattedDate, " ==="]).

print_elements(El, Type) ->
    Header = create_header(Type),
    Result = Header ++ lists:concat(gen_elements(El, [])),
    io:format(Result).

gen_elements([], Acc) -> Acc;
gen_elements([H|T], Acc) ->
    gen_elements(T, [string:concat("~n\t", H)|Acc]).
