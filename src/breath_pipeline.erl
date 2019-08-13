%% -----------------------------------------------------------------------------
%% @author   <pouriya.jahanbakhsh@gmail.com>
%% @version
%% @doc
%%           ROP (Railway Oriented Programming) parse transformation.
%% @end
%% -----------------------------------------------------------------------------
-module(breath_pipeline).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% 'compile' callbacks:
-export([parse_transform/2
        ,format_error/1]).

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(KEY , '$breath_pipeline').

%% -----------------------------------------------------------------------------
%% 'compile' callbacks:

%% @hidden
parse_transform(AST, _) ->
    transform(AST).


%% @hidden
format_error(argument) ->
    [
        "Except first argument, every argument of 'pipeline' must be a function"
        " call."
    ];

format_error(arguments) ->
    ["'pipeline' function must has at least two arguments."];

format_error(underscore) ->
    ["Every 'pipeline' function call can contain only one underscore."].

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
                {Mod, {Func, Arity}} when (
                                              Mod == breath andalso
                                              Func == pipeline
                                          ) orelse
                                          (
                                              Mod == breath_pipeline andalso
                                              Func == do
                                          ) ->
                    if
                        Arity > 1 ->
                            transform_application(Item);
                        true ->
                            _ = maybe_error({erl_syntax:get_pos(Item)
                                            ,?MODULE
                                            ,arguments}),
                            erl_syntax:revert(Item)
                    end;
                {breath_pipeline, Arity} ->
                    if
                        Arity > 1 ->
                            transform_application(Item);
                        true ->
                            _ = maybe_error({erl_syntax:get_pos(Item)
                                            ,?MODULE
                                            ,arguments}),
                            erl_syntax:revert(Item)
                    end;
                _ ->
                    erl_syntax:revert(Item)
            end;
        function ->
            erl_syntax:revert(maybe_have_error(Item));
        _ ->
            erl_syntax:revert(Item)
    end.


transform_application(App) ->
    [Arg | Args] = erl_syntax:application_arguments(App),
    case check_arguments(Args, erl_syntax:get_pos(App)) of
        ok ->
            case transform_applications_to_application(Args, Arg) of
                error ->
                    App;
                App2 ->
                    erl_syntax:revert(App2)
            end;
        _ ->
            App
    end.


transform_applications_to_application([App | Apps], Arg) ->
    case transform_application(App, Arg) of
        error ->
            error;
        App2 ->
            transform_applications_to_application(Apps, App2)
    end;

transform_applications_to_application([], App) ->
    App.


check_arguments([Arg|Args], Pos) ->
    case erl_syntax:type(Arg) of
        application ->
            case erl_syntax_lib:analyze_application(Arg) of
                {_, {_, Arity}} when erlang:is_integer(Arity) ->
                    check_arguments(Args, Pos);
                {_, Arity} when erlang:is_integer(Arity) ->
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
    case replace_arguments(Args, Value, [], false, Pos) of
        Args2 when erlang:is_list(Args2) ->
            NewApp = erl_syntax:application(
                erl_syntax:application_operator(App),
                Args2
            ),
            erl_syntax:set_pos(NewApp, Pos);
        Err ->
            Err
    end.


replace_arguments([Arg|Args], Value, Args2, IsReplaced, AppPos) ->
    case erl_syntax:type(Arg) of
        underscore when not IsReplaced ->
            Pos = erl_syntax:get_pos(Arg),
            Arg2 = erl_syntax:set_pos(Value, Pos),
            replace_arguments(Args, Value, [Arg2|Args2], true, AppPos);
        underscore ->
            maybe_error({erl_syntax:get_pos(Arg), ?MODULE, underscore});
        _ ->
            replace_arguments(Args, Value, [Arg|Args2], IsReplaced, AppPos)
    end;
replace_arguments(_, Value, [], false, AppPos) ->
    [erl_syntax:set_pos(Value, AppPos)];

replace_arguments(_, Value, [LastArg | _]=Args2, false, _) ->
    lists:reverse(
        [erl_syntax:set_pos(Value, erl_syntax:get_pos(LastArg)) | Args2]
    );

replace_arguments(_, _, Args2, _, _) ->
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
