%% @author Nikolay Mavrenkov <koluch@koluch.ru>
%% @copyright 2013 Nikolay Mavrenkov

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
    {ok, [Routes]} = file:consult(SourceFile),
    Forms = eroutes_translate:rules_to_erl(Routes,ModuleName),
    eroutes_translate:compile_forms(ModuleName,Forms),
    ModuleName.

%% @doc Read routes file and generate corresponding erlang module file
-spec file_to_file(SourceFile::string(),ModuleName::atom(),OutputFile::string()) -> ok | terminates.
file_to_file(SourceFile, ModuleName, OutputFile) ->
    {ok, [Routes]} = file:consult(SourceFile),
    Forms = eroutes_translate:rules_to_erl(Routes,ModuleName),
    {ok, File} = file:open(OutputFile, [write]),
    lists:map(fun(Line) -> file:write(File, Line), file:write(File, [$\n, $\n]) end, Forms),
    file:close(File),
    ok.
