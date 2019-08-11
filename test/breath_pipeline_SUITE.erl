%% -----------------------------------------------------------------------------
-module(breath_pipeline_SUITE).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% ct callbacks:
-export([init_per_suite/1
        ,end_per_suite/1
        ,all/0
        ,init_per_testcase/2
        ,end_per_testcase/2]).

%% Testcases:
-export(['1'/1
        ,'2'/1
        ,'3'/1
        ,'4'/1
        ,'5'/1
        ,'6'/1
        ,'7'/1
        ,'8'/1
        ,'9'/1
        ,'10'/1]).

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------------
%% ct callbacks:


all() ->
    IsInteger =
        fun(Func) ->
            try
                _ = erlang:list_to_integer(erlang:atom_to_list(Func)),
                true
            catch
                _:_ ->
                    false
            end
        end,
    % '1', '2', ...
    lists:sort([Func || {Func, Arity} <- ?MODULE:module_info(exports)
        ,Arity == 1 andalso IsInteger(Func)]).


init_per_suite(Cfg) ->
    application:start(sasl),
    Cfg.


end_per_suite(Cfg) ->
    application:stop(sasl),
    Cfg.


init_per_testcase(_TestCase, Cfg) ->
    Cfg.


end_per_testcase(_TestCase, _Cfg) ->
    ok.

%% -----------------------------------------------------------------------------
%% Test cases:


'1'(Cfg) ->
    Src = <<
        "-module(breath_pipeline_).\n "
        "-compile({parse_transform, breath_pipeline}).\n "
        "f() -> breath:pipeline(1, f2(_)).\n "
        "f2(Int) -> Int+1.\n "
          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_pipeline_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({ok, _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_pipeline}, return_errors, {outdir, Dir}])),
    ?assertMatch(2, breath_pipeline_:f()),
    _ = code:del_path(Dir),
    _ = code:purge(breath_pipeline_),
    true = code:delete(breath_pipeline_),
    _ = file:delete(File),
    ok.


'2'(Cfg) ->
    Src = <<
        "-module(breath_pipeline_).\n "
        "-compile({parse_transform, breath_pipeline}).\n "
        "f() -> breath:pipeline(1, f2(_), ?MODULE:f3(5, 1, _)).\n "
        "f2(Int) -> Int+1.\n "
        "f3(Int1, Int2, Int3) -> Int1*2+Int2+Int3.\n "
          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_pipeline_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({ok, _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_pipeline}, return_errors, {outdir, Dir}])),
    ?assertMatch(13, breath_pipeline_:f()),
    _ = code:del_path(Dir),
    _ = code:purge(breath_pipeline_),
    true = code:delete(breath_pipeline_),
    _ = file:delete(File),
    ok.



'3'(Cfg) ->
    Src = <<
        "-module(breath_pipeline_).\n "
        "-compile({parse_transform, breath_pipeline}).\n "
        "f(X) -> breath:pipeline(X, erlang:length()).\n "
        "f2(X) -> breath:pipeline(X, lists:seq(1)).\n "
          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_pipeline_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({ok, _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_pipeline}, return_errors, {outdir, Dir}])),
    ?assertMatch(5, breath_pipeline_:f([1,2,3,4,5])),
    ?assertMatch([1,2,3,4,5], breath_pipeline_:f2(5)),
    _ = code:del_path(Dir),
    _ = code:purge(breath_pipeline_),
    true = code:delete(breath_pipeline_),
    _ = file:delete(File),
    ok.



'4'(Cfg) ->
    Src = <<
        "-module(breath_pipeline_).\n "
        "-compile({parse_transform, breath_pipeline}).\n "
        "f() -> breath:pipeline(1).\n "
        "f2(Int) -> Int+1.\n "
          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_pipeline_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({error, [{_, [{3,breath_pipeline,arguments}]}], _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_pipeline}, return_errors, {outdir, Dir}])),
    _ = code:del_path(Dir),
    _ = code:purge(breath_pipeline_),
    _ = file:delete(File),
    true = code:delete(breath_pipeline_),
    _ = file:delete(File),
    ok.



'5'(Cfg) ->
    Src = <<
        "-module(breath_pipeline_).\n "
        "-compile({parse_transform, breath_pipeline}).\n "
        "f() -> breath:pipeline(1
                          ,1
                          ,1
                          ,foo).\n "
          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_pipeline_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({error, [{_, [{4,breath_pipeline,argument}]}], _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_pipeline}, return_errors, {outdir, Dir}])),
    _ = code:del_path(Dir),
    _ = code:purge(breath_pipeline_),
    _ = file:delete(File),
    true = code:delete(breath_pipeline_),
    _ = file:delete(File),
    ok.



'6'(Cfg) ->
    Src = <<
        "-module(breath_pipeline_).\n "
        "-compile({parse_transform, breath_pipeline}).\n "
        "f() -> breath:pipeline(1
                          ,breath:pipeline(1,1,1)
                          ,1
                          ,foo).\n "
          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_pipeline_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({error, [{_, [{4,breath_pipeline,argument}]}], _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_pipeline}, return_errors, {outdir, Dir}])),
    _ = code:del_path(Dir),
    _ = code:purge(breath_pipeline_),
    _ = file:delete(File),
    true = code:delete(breath_pipeline_),
    _ = file:delete(File),
    ok.



'7'(Cfg) ->
    Src = <<
        "-module(breath_pipeline_).\n "
        "-compile({parse_transform, breath_pipeline}).\n "
        "f(Int) -> \n "
        "    case breath:pipeline(Int, f2(_)) of\n"
        "        2 ->\n "
        "            breath:pipeline(2, f3(_));\n "
        "        3=Err ->\n "
        "            Err\n "
        "    end.\n "
        "f2(1) -> 2;\n "
        "f2(_) -> 3.\n "
        "f3(X) -> X*2.\n "

          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_pipeline_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({ok, _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_pipeline}, return_errors, {outdir, Dir}])),
    ?assertMatch(4, breath_pipeline_:f(1)),
    ?assertMatch(3, breath_pipeline_:f(2)),
    _ = code:del_path(Dir),
    _ = code:purge(breath_pipeline_),
    true = code:delete(breath_pipeline_),
    _ = file:delete(File),
    ok.



'8'(Cfg) ->
    Src = <<
        "-module(breath_pipeline_).\n "
        "-compile({parse_transform, breath_pipeline}).\n "
        "f(Int) -> \n "
        "    breath:pipeline(\n"
        "               Int\n "
        "              ,f2(_)\n "
        "              ,f2(_)\n "
        "              ).\n "
        "f2(1) -> ok;\n"
        "f2(ok) -> 2.\n "
          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_pipeline_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({ok, _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_pipeline}, return_errors, {outdir, Dir}])),
    ?assertError(function_clause, breath_pipeline_:f(2)),
    _ =
        try
            breath_pipeline_:f(2)
        catch
            _:function_clause:Stacktrace ->
                ?assertMatch([{breath_pipeline_, f2, [2], [_, {line, 9}]}|_], Stacktrace)
        end,

    ?assertError(function_clause, breath_pipeline_:f(2)),
    _ =
        try
            breath_pipeline_:f(2)
        catch
            _:function_clause:Stacktrace2 ->
                ?assertMatch([{breath_pipeline_, f2, [2], [_, {line, 9}]}|_], Stacktrace2)
        end,

    _ = code:del_path(Dir),
    _ = code:purge(breath_pipeline_),
    true = code:delete(breath_pipeline_),
    _ = file:delete(File),
    ok.



'9'(Cfg) ->
    Src = <<
        "-module(breath_pipeline_).\n "
        "-compile({parse_transform, breath_pipeline}).\n "
        "f() -> breath:pipeline(1, f2(_, _)).\n "
          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_pipeline_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({error, [{_, [{3,breath_pipeline,underscore}]}], _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_pipeline}, return_errors, {outdir, Dir}])),
    _ = code:del_path(Dir),
    _ = code:purge(breath_pipeline_),
    true = code:delete(breath_pipeline_),
    _ = file:delete(File),
    ok.


'10'(_) ->
    ?assertMatch([_|_], breath_pipeline:format_error(argument)),
    ?assertMatch([_|_], breath_pipeline:format_error(arguments)),
    ?assertMatch([_|_], breath_pipeline:format_error(underscore)),
    ok.