-module(rooster_logger).

-export([info/1, err/1]).

%% log info 
%%
-spec info(list(string())) -> 'ok'.

info(El) ->
    print_elements(El, "INFO").

%% log error
%%
-spec err(list(string())) -> 'ok'.

err(El) ->
    print_elements(El, "ERROR").

%% create header
%%
-spec create_header(string()) -> string().

create_header(Type) ->
    {{Year, Month, Day}, {Hour, Min, Second}} = calendar:local_time(),
    FormattedDate = lists:concat([Month, "-", Day, "-", Year, "::", Hour, ":", Min, ":", Second]),
    lists:concat(["~n= ", Type, " === ", FormattedDate, " ==="]).

%% print elements
%%
-spec print_elements(list(string()), string()) -> 'ok'.

print_elements(El, Type) ->
    Header = create_header(Type),
    Result = Header ++ lists:concat(gen_elements(El, [])),
    io:format(Result).

%% pring each element in a new line
%%
-spec gen_elements(list(string()), list(string())) -> list(string()).

gen_elements([], Acc) -> Acc;
gen_elements([H|T], Acc) ->
    gen_elements(T, [string:concat("~n\t", H)|Acc]).
