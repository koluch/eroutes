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
%% Created : 12 Oct 2013 by Nikolay Mavrenkov <koluch@koluch.ru>
%%--------------------------------------------------------------------
-module(controller_for_test).

-export([all_posts/0, new_post/1, show_post/1, show_comment/2, show_404/0, show_index/0]).

all_posts () -> "All posts list".
new_post (Request) -> "New post title: " ++ proplists:get_value(title, Request).
show_index() -> "Posts index".
show_post (Number) -> "Content of post #" ++ Number.
show_comment(Post,Comment) -> "Content of comment #" ++ Comment ++ " for post #" ++ Post.
show_404() -> "Page 404 content".
     



