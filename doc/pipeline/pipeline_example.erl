-module(pipeline_example).

-compile({parse_transform, breath_pipeline}).
-compile(export_all).
-compile(nowarn_export_all).


test(Int) ->
    breath:pipeline(       % Also we can use breath_pipeline(...)
        Int,
        task_1(),          % task_1(Int)
        task_2(_, Int),    % task_2(task_1(Int), Int)
        task_3(10, _, 10), % task_3(10, task_2(task_1(Int), Int), 10)
        format(),          % format(task_3(10, task_2(task_1(Int), Int), 10))
        io:format()        % io:format(format(task_3(10, task_2(task_1(Int), Int), 10)))
    ).


task_1(X) ->
    X * 2.


task_2(Int1, Int2) ->
    Int1 * Int2.


task_3(Int1, Int2, Int3) ->
    Int1 + Int2 * Int3.


format(Int) ->
    "Number is " ++ erlang:integer_to_list(Int) ++ "\n".


crash_test() ->
    breath:pipeline(
        {},
        lists:keyfind(key, 1) % proplists:keyfind(key, 1, {})
    ).


crash_test(Reason) ->
    breath:pipeline(
        Reason,
        io_lib:print(),
        erlang:error()
    ).


compile_test_1() ->
    breath:pipeline(task_1()).



compile_test_2() ->
    breath:pipeline(
        init_argument, 
        bad_function_call
    ).

compile_test_3() ->
    breath:pipeline(
        init_argument, 
        task_3(_, 2, _)
    ).
