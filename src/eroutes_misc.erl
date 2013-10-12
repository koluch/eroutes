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
-module(eroutes_misc).
-author("Nikolay Mavrenkov <koluch@koluch.ru>").

%% API
-export([record_to_proplist/2, filter_empty_strings/1]).

record_to_proplist(Record, Keys) ->
    [_Name|Values] = tuple_to_list(Record),
    lists:zip(Keys,Values).

filter_empty_strings(List) -> lists:filter(fun(Next) -> case Next of "" -> false; _ -> true end end, List).
