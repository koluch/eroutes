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
-module(eroutes).
-author("Nikolay Mavrenkov <koluch@koluch.ru>").

%% Application callbacks
-export([file_to_memory/2, file_to_file/3]).

%% -spec eroutes:file_to_file(SourceFile::string(),ModuleName::atom(),OutputFile::atom()) -> string().

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% @doc Read routes file and load as erlang module to memory
-spec file_to_memory(SourceFile::string(),ModuleName::atom()) -> {'ok', ModuleName::atom()} | terminates .
file_to_memory(SourceFile, ModuleName) ->
    try
        {ok, [Routes]} = file:consult(SourceFile),
        Forms = eroutes_translate:rules_to_erl(Routes,ModuleName),
        eroutes_translate:compile_forms(ModuleName,Forms),
        {ok, ModuleName}
    catch
        throw:E -> {error, E, erlang:get_stacktrace()};
        error:E -> {error, E, erlang:get_stacktrace()}
    end.

%% @doc Read routes file and generate corresponding erlang module file
-spec file_to_file(SourceFile::string(),ModuleName::atom(),OutputFile::string()) -> ok | terminates.
file_to_file(SourceFile, ModuleName, OutputFile) ->
    try
        {ok, [Routes]} = file:consult(SourceFile),
        Forms = eroutes_translate:rules_to_erl(Routes,ModuleName),
        {ok, File} = file:open(OutputFile, [write]),
        lists:map(fun(Line) -> file:write(File, Line), file:write(File, [$\n, $\n]) end, Forms),
        file:close(File),
        ok
    catch
        _:e -> {error, e}
    end.
