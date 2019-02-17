%% -----------------------------------------------------------------------------
%% @author   <pouriya.jahanbakhsh@gmail.com>
%% @version
%% @doc
%%           ROP (Railway Oriented Programming) parse transformation.
%% @end
%% -----------------------------------------------------------------------------
-module(breath_rop).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% 'compile' callbacks:
-export([parse_transform/2
        ,format_error/1]).

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(KEY , '$breath_rop').

%% -----------------------------------------------------------------------------
%% 'compile' callbacks:

%% @hidden
parse_transform(AST, _) ->
    transform(AST).


%% @hidden
format_error(argument) ->
    ["Each argument of 'rop' function must be a function call."];

format_error(arguments) ->
    ["'rop' function must has at least two arguments."].

%% -----------------------------------------------------------------------------
%% Internals:

transform([Item | AST]) ->
    [erl_syntax_lib:map(fun transform_rop/1, Item) | transform(AST)];

transform(_) ->
    [].


transform_rop(Item) ->
    case erl_syntax:type(Item) of
        application ->
            case erl_syntax_lib:analyze_application(Item) of
                {breath, {rop, Arity}} when erlang:is_integer(Arity) andalso
                                            Arity > 1 ->
                    transform_application(Item);
                {breath_rop, Arity} when erlang:is_integer(Arity) andalso
                                         Arity > 1 ->
                    transform_application(Item);
                {X, Y} when (X == breath andalso
                             erlang:element(1, Y) == rop
                            ) orelse
                            (X == breath_rop andalso
                             erlang:is_integer(Y)
                            ) ->
                    _ = maybe_error({erl_syntax:get_pos(Item)
                                    ,?MODULE
                                    ,arguments}),
                    erl_syntax:revert(Item);
                _ ->
                    erl_syntax:revert(Item)
            end;
        function ->
            erl_syntax:revert(maybe_have_error(Item));
        _ ->
            erl_syntax:revert(Item)
    end.


transform_application(App) ->
    Args = erl_syntax:application_arguments(App),
    case check_arguments(Args, erl_syntax:get_pos(App)) of
        ok ->
            erl_syntax:revert(transform_applications_to_cases(Args, undefined));
        _ ->
            App
    end.


transform_applications_to_cases([Arg], Value) ->
    transform_application(Arg, Value);

transform_applications_to_cases([Arg|Args], Value) ->
    Ok = erl_syntax:copy_pos(Arg, erl_syntax:atom(ok)),
    OkResult = erl_syntax:copy_pos(Arg, random_variable("_Result_")),
    OkTuple = erl_syntax:copy_pos(Arg, erl_syntax:tuple([Ok, OkResult])),

    Err = erl_syntax:copy_pos(Arg, erl_syntax:atom(error)),
    ErrInfo = erl_syntax:copy_pos(Arg, random_variable("_Info_")),
    ErrTuple = erl_syntax:copy_pos(Arg, erl_syntax:tuple([Err, ErrInfo])),

    Clauses =
        [erl_syntax:copy_pos(
            Arg,
            erl_syntax:clause(
                [OkTuple],
                none,
                [transform_applications_to_cases(Args, OkResult)]
            )
         ),
         erl_syntax:copy_pos(
            Arg,
            erl_syntax:clause([ErrTuple], none, [ErrTuple])
         )
        ],
    Arg2 =
        if
            Value /= undefined ->
                transform_application(Arg, Value);
            true ->
                Arg
        end,
    erl_syntax:revert(
        erl_syntax:copy_pos(Arg, erl_syntax:case_expr(Arg2, Clauses))
    ).


random_variable(Pre) ->
    {Mega, Sec, Micro} = os:timestamp(),
    Timestamp = ((Mega * 1000000) + Sec) * 1000000 + Micro,
    erl_syntax:variable(Pre ++ erlang:integer_to_list(Timestamp)).


check_arguments([Arg|Args], Pos) ->
    case erl_syntax:type(Arg) of
        application ->
            case erl_syntax_lib:analyze_application(Arg) of
                {_, {_, Arity}} when erlang:is_integer(Arity) ->
                    check_arguments(Args, Pos);
                {_, Arity} when erlang:is_integer(Arity) ->
                    check_arguments(Args, Pos);
                Arity when erlang:is_integer(Arity) ->
                    check_arguments(Args, Pos);
                _ ->
                    maybe_error({erl_syntax:get_pos(Arg), ?MODULE, argument})
            end;
        _ ->
            maybe_error({erl_syntax:get_pos(Arg), ?MODULE, argument})
    end;
check_arguments(_, _) ->
    ok.


transform_application(App, Value) ->
    Pos = erl_syntax:get_pos(App),
    Args = erl_syntax:application_arguments(App),
    Args2 = replace_arguments(Args, Value, []),
    NewApp = erl_syntax:application(
        erl_syntax:application_operator(App),
        Args2
    ),
    erl_syntax:set_pos(NewApp, Pos).


replace_arguments([Arg|Args], Value, Args2) ->
    case erl_syntax:type(Arg) of
        underscore ->
            Pos = erl_syntax:get_pos(Arg),
            Arg2 = erl_syntax:set_pos(Value, Pos),
            replace_arguments(Args, Value, [Arg2|Args2]);
        _ ->
            replace_arguments(Args, Value, [Arg|Args2])
    end;
replace_arguments(_, _, Args2) ->
    lists:reverse(Args2).


maybe_error(Err) ->
    _ =
        case erlang:get(?KEY) of
            undefined ->
                erlang:put(?KEY, erl_syntax:error_marker(Err));
            _ ->
                ok
        end,
    error.


maybe_have_error(Item) ->
    case erlang:get(?KEY) of
        undefined ->
            Item;
        Item2 ->
            _ = erlang:erase(?KEY),
            Item2
    end.