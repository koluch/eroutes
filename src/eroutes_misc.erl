%% @author Nikolay Mavrenkov <koluch@koluch.ru>
%% @copyright 2013 Nikolay Mavrenkov

-module(eroutes_misc).
-author("Nikolay Mavrenkov <koluch@koluch.ru>").

%% API
-export([record_to_proplist/2, filter_empty_strings/1]).

record_to_proplist(Record, Keys) ->
    [_Name|Values] = tuple_to_list(Record),
    lists:zip(Keys,Values).

filter_empty_strings(List) -> lists:filter(fun(Next) -> case Next of "" -> false; _ -> true end end, List).