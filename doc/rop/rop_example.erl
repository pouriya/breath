-module(rop_example).

-compile({parse_transform, breath_rop}).
-compile(export_all).
-compile(nowarn_export_all).


test(Arg) ->
    breath_rop(      % Also we can use breath:rop
        task_1(Arg), % If yields {ok, R}, R will be replaced with _ in next call
        task_2(_),
        task_3(_, 10),
        task_4(_, 2, _),
        ?MODULE:task_5(_),
        (fun(X) -> task_6(X) end)(_)
    ).
%% Above code can be written like:
%%test(Arg) ->
%%    case task_1(Arg) of
%%        {ok, Arg2} ->
%%            case task_2(Arg2) of
%%                {ok, Arg3} ->
%%                    case task_3(Arg3, 10) of
%%                        {ok, Arg4} ->
%%                            case task_4(Arg4, 2, Arg4) of
%%                                {ok, Arg5} ->
%%                                    case ?MODULE:task_5(Arg5) of
%%                                        {ok, Arg6} ->
%%                                            (fun(X) -> task_6(X) end)(Arg6);
%%                                        {error, _}=Err ->
%%                                            Err
%%                                    end;
%%                                {error, _}=Err ->
%%                                    Err
%%                            end;
%%                        {error, _}=Err ->
%%                            Err
%%                    end;
%%                {error, _}=Err ->
%%                    Err
%%            end;
%%        {error, _}=Err ->
%%            Err
%%    end.


task_1(X) when erlang:is_integer(X) ->
    {ok, X};
task_1(X) ->
    {error, {type, X}}.


task_2(Int) when Int rem 2 == 0 ->
    {ok, Int};
task_2(Int) ->
    {error, {odd, Int}}.


task_3(Int, Int2) ->
    {ok, Int * Int2}.


task_4(Int1, Int2, Int3) ->
    {ok, erlang:integer_to_list(Int1 * Int2) ++ "." ++ erlang:integer_to_list(Int3)}.


task_5(Int) ->
    {ok, erlang:list_to_float(Int)}.


task_6(Float) ->
    io:format("Number: ~p~n", [Float]).
