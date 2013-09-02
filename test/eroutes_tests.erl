%% @author Nikolay Mavrenkov <koluch@koluch.ru>
%% @copyright 2008 Nikolay Mavrenkov

-module(eroutes_tests).
-author("Nikolay Mavrenkov <koluch@koluch.ru>").

-include_lib("eunit/include/eunit.hrl").

compile_to_memory_test() ->
    ?assertEqual({ok, test_routes_module}, eroutes:file_to_memory("../priv/routes.test", test_routes_module)),
    ?assertEqual("/posts/42/comments/666", test_routes_module:create(comment, [{post, 42}, {comment, 666}])).

compile_to_file_test() ->
    ?assertEqual(ok, eroutes:file_to_file("../priv/routes.test", test_routes_module, "test_routes_module.erl")),
    ?assertEqual({ok, test_routes_module},compile:file(test_routes_module)),
    ?assertEqual({module, test_routes_module},code:load_file(test_routes_module)),
    ?assertEqual("/posts/42/comments/666", test_routes_module:create(comment, [{post, 42}, {comment, 666}])).
