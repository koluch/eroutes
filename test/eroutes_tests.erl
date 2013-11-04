%% -*- coding: utf-8; erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%--------------------------------------------------------------------
%% @author Nikolay Mavrenkov <koluch@koluch.ru>
%% @copyright (C) 2013, Nikolay Mavrenkov
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
%% Copyright 2013 Nikolay Mavrenkov
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
%% Created : 13 Oct 2013 by Nikolay Mavrenkov <koluch@koluch.ru>
%%--------------------------------------------------------------------
-module(eroutes_tests).
-author("Nikolay Mavrenkov <koluch@koluch.ru>").

-include_lib("eunit/include/eunit.hrl").

compile_to_memory_test() ->
    ?assertEqual({ok, test_routes_module}, eroutes:file_to_memory("../priv/routes.test", test_routes_module)),

    %% Handle
    ?assertEqual("Content of post #42", test_routes_module:handle("/posts/42/")),
    ?assertEqual("Content of comment #13 for post #42", test_routes_module:handle("/posts/42/comments/13")),
    ?assertEqual("Page 404 content", test_routes_module:handle("something")),
    ?assertEqual("Page 404 content", test_routes_module:handle("")),
    ?assertEqual("Posts index", test_routes_module:handle("/posts/some/another/path", ["It is context"])),

    %% Create
    ?assertEqual("/posts/42/comments/666", test_routes_module:create(comment, [{post, 42}, {comment, 666}])).

compile_to_file_test() ->
    ?assertEqual(ok, eroutes:file_to_file("../priv/routes.test", test_routes_module, "test_routes_module.erl")),
    ?assertEqual({ok, test_routes_module},compile:file(test_routes_module)),
    ?assertEqual({module, test_routes_module},code:load_file(test_routes_module)),
    ?assertEqual("/posts/42/comments/666", test_routes_module:create(comment, [{post, 42}, {comment, 666}])).
