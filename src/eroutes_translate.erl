%% @author Nikolay Mavrenkov <koluch@koluch.ru>
%% @copyright 2008 Nikolay Mavrenkov

-module(eroutes_translate).
-author("Nikolay Mavrenkov <koluch@koluch.ru>").

%% API
-export([rules_to_erl/2, compile_forms/2]).

rules_to_erl(Rules,ModuleName) ->
    check_format_rules(Rules),
    HeaderForms = generate_header(ModuleName),
    HandleForms = generate_handle_forms(Rules),
    CreateForms = generate_create_forms(Rules),
    UtilsForms = generate_utils_forms(),
    lists:append([HeaderForms,HandleForms,CreateForms,UtilsForms]).


compile_forms(ModuleName,Forms) ->
    TokenizedForms = [CompiledForm || {ok, CompiledForm, _} <- [erl_scan:string(StringForm) || StringForm <- Forms]],
    ParsedForms = [ ParsedForm || {ok, ParsedForm} <-[ erl_parse:parse_form(TokenizedForm) || TokenizedForm <- TokenizedForms]],
    {ok, ModuleName, Bin} = compile:forms(ParsedForms, [{{warn_format, 0}}]),
    code:load_binary(ModuleName, "nofile", Bin).


%% Check format
check_format_rules(Routes) ->
    case typeof(Routes) of
        list -> check_format_rules_routes_each(Routes);
        Type -> throw({"Routes should be a list!", Routes,{wront_type,Type}})
    end.

check_format_rules_routes_each([]) -> ok;
check_format_rules_routes_each([Route|Rest]) ->
    sure_tuple(Route,"Each route should be a tuple!"),
    check_format_rules_route_details(Route),
    check_format_rules_routes_each(Rest).

check_format_rules_route_details({Name, Path, MFA}) ->
    check_format_rules_name(Name),
    check_format_rules_path(Path),
    check_format_rules_mfa(MFA);

check_format_rules_route_details(Else) -> throw({"Each route should have following format: {Name, Path, MFA}!", Else}).

check_format_rules_name(Name) -> sure_atom(Name, "Name should be an atom!").

check_format_rules_path(Path) ->
    sure_list(Path, "Path should be a list!"),
    check_format_rules_path_elements(Path).

check_format_rules_path_elements(["*"]) -> ok;
check_format_rules_path_elements([]) -> ok;
check_format_rules_path_elements([Element|Rest]) ->
    case typeof(Element) of
        atom -> ok;
        list -> case is_string(Element) of
                    true -> case Element of
                                "*" -> throw({"'*' element could be only the last element of path!"}); %% todo: need to show whole rule here
                                _ -> ok
                            end;
                    _ -> throw({"Each path element should either atom or string!", {Element, list}})
                end;
        WrongType -> throw({"Each path element should either atom or string!", {Element, WrongType}})
    end,
    check_format_rules_path_elements(Rest).


check_format_rules_mfa({Module,Function,Arguments}) ->
    sure_atom(Module,"Module name should be an atom"),
    sure_atom(Function,"Function should be an atom"),
    sure_list(Arguments,"Arguments should be list");
check_format_rules_mfa(Else) -> throw({"Module call should have following wormat: {Module,Function,Arguments}!", Else}).



%% Type check utils
sure_atom(Term,Msg) -> sure_type(Term,atom,Msg).
sure_list(Term,Msg) -> sure_type(Term,list,Msg).
sure_tuple(Term,Msg) -> sure_type(Term,tuple,Msg).
sure_type(Term,Type,Msg) -> case typeof(Term) of Type -> ok; WrongType -> throw({Msg, {Term,WrongType}}) end.


%% Header
generate_header(ModuleName) ->
    [
        "-module("++atom_to_list(ModuleName)++").",
        "-export([handle/2,create/2])."
      ].

%% Handle
generate_handle_forms(Routes) ->
    Result = [generate_handler_form(Rule) || Rule <- Routes],
    [string:join(Result, ";\n") ++ "."].

generate_handler_form(_Route = {_RouteName,PathDef,MFA}) ->
    string:join(eroutes_misc:filter_empty_strings([
        "\nhandle(" ++ generate_handle_match_pattern(PathDef) ++ ", Request) -> ",
        generate_req_calls(MFA),
        generate_handle_call(MFA)
    ]), "\n    ").

generate_handle_match_pattern(Terms) ->
    Tmp1 = generate_term_representation(Terms),
    case string:right(Tmp1,3) of
        ",*]" -> string:left(Tmp1, string:len(Tmp1)-3) ++ "|_]";
        "[*]" -> "[_|_]";
        _ -> Tmp1
    end.

generate_handle_call({Module,Function,Arguments}) ->
    PreparedArguments = lists:map(fun(Arg) -> generate_term_representation(Arg) end, Arguments),
    atom_to_list(Module) ++ ":" ++ atom_to_list(Function) ++ "("++string:join(PreparedArguments, ", ")++")".

generate_req_calls(_MFA = {_Module,_Function,Arguments}) ->
    OnlyReq = lists:filter(fun(Next) ->
        case typeof(Next) of
            atom ->
                case string:left(atom_to_list(Next),4) of
                        "req_" -> true;
                        _ -> false
                    end;
            _ -> false
        end
    end, Arguments),
    ToStrings = [{atom_to_list(X),string:substr(atom_to_list(X),5)} || X <- OnlyReq],
    ProplistGetValue = [first_upper(Full) ++ " = proplists:get_value(" ++ Cut ++ ", Request)," || {Full,Cut} <- ToStrings],
    string:join(ProplistGetValue, "\n    ").



%% Create

%% create(comment, Params) -> "/" ++ "posts" ++ "/" ++ get_param(number,Params) ++ "/" ++ "comments" ++ get_param(cnum,Params).

generate_create_forms(Routes) ->
    Result = [generate_create_form(Rule) || Rule <- Routes],
    [string:join(Result, ";\n") ++ "."].

generate_create_form(_Rule = {RouteName,PathDef,_MFA}) ->
    "create("++atom_to_list(RouteName)++", Params) -> " ++ generate_create_body(PathDef).


generate_create_body(PathDef) ->
    case typeof(PathDef) of
        list ->
            Parts = lists:map(fun(NextEl) ->
                case typeof(NextEl) of
                    atom ->  "check_type(proplists:get_value("++atom_to_list(NextEl)++",Params))";
                    list -> "\"" ++ NextEl ++ "\""
                end
            end, PathDef),
            PartsFiltered = lists:filter(fun(Next) -> case Next of "\"*\"" -> false; _ -> true end end, Parts),
            "[$/|string:join([" ++ string:join(PartsFiltered, ",") ++ "], \"/\")]";
        _ -> "throw(not_implemented)"
    end.



%% Generated utils


generate_utils_forms() ->
    [
        "check_type(Term) ->
    case typeof(Term) of
        atom -> atom_to_list(Term);
        integer -> integer_to_list(Term);
        binary -> binary_to_list(Term);
        _ -> Term
    end.",
        "typeof(Term) ->
    case is_atom(Term) of
        true -> atom;
        Else -> case is_list(Term) of
                    true -> list;
                    Else -> case is_integer(Term) of
                                true -> integer;
                                Else -> case is_binary(Term) of
                                            true -> binary;
                                            Else -> else
                                        end
                            end
                end
    end."
    ].



%%%% Local util functions
generate_term_representation(Term) ->
    case typeof(Term) of
        atom -> first_upper(atom_to_list(Term));
        list -> case is_string(Term) of
                    true -> Term;
                    _ -> SubTerms = [generate_term_representation(Next) || Next <- Term],
                        lists:append(["[",string:join(SubTerms,","),"]"])
                end;
        integer -> integer_to_list(Term);
        float -> float_to_list(Term);
        tuple ->
            AsList = tuple_to_list(Term),
            PreparedTupleContent = [generate_term_representation(T)|| T <- AsList],
            AsString = string:join(PreparedTupleContent, ","),
            lists:append(["{",AsString,"}"])
    end.

first_upper([First|Rest]) -> string:to_upper([First]) ++ Rest.

typeof(Term) ->
    case is_atom(Term) of
        true -> atom;
        Else -> case is_list(Term) of
                    true -> list;
                    Else -> case is_integer(Term) of
                                true -> integer;
                                Else -> case is_float(Term) of
                                            true -> float;
                                            Else -> case is_tuple(Term) of
                                                        true -> tuple;
                                                        Else -> other
                                                    end
                                        end
                            end
                end
    end.

is_string([]) -> true;
is_string([H|_]) when not(is_integer(H)) or (H < 32) or (H>126) -> false;
is_string([_|T]) -> is_string(T);
is_string(_) -> false.
