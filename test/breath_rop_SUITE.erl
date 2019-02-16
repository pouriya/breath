%% -----------------------------------------------------------------------------
-module(breath_rop_SUITE).
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
        ,'9'/1]).

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
        "-module(breath_rop_).\n "
        "-compile({parse_transform, breath_rop}).\n "
        "f() -> breath:rop(1, f2(_)).\n "
        "f2(Int) -> {ok, Int+1}.\n "
          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_rop_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({ok, _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_rop}, return_errors, {outdir, Dir}])),
    ?assertMatch({ok, 2}, breath_rop_:f()),
    _ = code:del_path(Dir),
    _ = code:purge(breath_rop_),
    true = code:delete(breath_rop_),
    _ = file:delete(File),
    ok.


'2'(Cfg) ->
    Src = <<
        "-module(breath_rop_).\n "
        "-compile({parse_transform, breath_rop}).\n "
        "f() -> breath:rop(1\n "
        "                 ,f2(_)\n "
        "                 ,?MODULE:f3(_, 1, _)\n "
        "                 ,(fun(X) -> f2(X) end)(_)).\n "
        "f2(Int) -> {ok, Int+1}.\n "
        "f3(Int1, Int2, Int1) -> {ok, Int1*2+Int2}.\n "
          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_rop_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({ok, _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_rop}, return_errors, {outdir, Dir}])),
    ?assertMatch({ok, 6}, breath_rop_:f()),
    _ = code:del_path(Dir),
    _ = code:purge(breath_rop_),
    true = code:delete(breath_rop_),
    _ = file:delete(File),
    ok.



'3'(Cfg) ->
    Src = <<
        "-module(breath_rop_).\n "
        "-compile({parse_transform, breath_rop}).\n "
        "f() -> breath:rop(1, f2()).\n "
          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_rop_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({error, [{_, [{3,breath_rop,argument}]}], _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_rop}, return_errors, {outdir, Dir}])),
    _ = code:del_path(Dir),
    _ = code:purge(breath_rop_),
    true = code:delete(breath_rop_),
    _ = file:delete(File),
    ok.



'4'(Cfg) ->
    Src = <<
        "-module(breath_rop_).\n "
        "-compile({parse_transform, breath_rop}).\n "
        "f() -> breath:rop(1).\n "
        "f2(Int) -> {ok, Int+1}.\n "
          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_rop_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({error, [{_, [{3,breath_rop,arguments}]}], _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_rop}, return_errors, {outdir, Dir}])),
    _ = code:del_path(Dir),
    _ = code:purge(breath_rop_),
    _ = file:delete(File),
    true = code:delete(breath_rop_),
    _ = file:delete(File),
    ok.



'5'(Cfg) ->
    Src = <<
        "-module(breath_rop_).\n "
        "-compile({parse_transform, breath_rop}).\n "
        "f() -> breath:rop(1
                          ,1
                          ,1
                          ,foo).\n "
          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_rop_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({error, [{_, [{4,breath_rop,argument}]}], _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_rop}, return_errors, {outdir, Dir}])),
    _ = code:del_path(Dir),
    _ = code:purge(breath_rop_),
    _ = file:delete(File),
    true = code:delete(breath_rop_),
    _ = file:delete(File),
    ok.



'6'(Cfg) ->
    Src = <<
        "-module(breath_rop_).\n "
        "-compile({parse_transform, breath_rop}).\n "
        "f() -> breath:rop(1
                          ,breath:rop(1,1,1)
                          ,1
                          ,foo).\n "
          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_rop_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({error, [{_, [{4,breath_rop,argument}]}], _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_rop}, return_errors, {outdir, Dir}])),
    _ = code:del_path(Dir),
    _ = code:purge(breath_rop_),
    _ = file:delete(File),
    true = code:delete(breath_rop_),
    _ = file:delete(File),
    ok.



'7'(Cfg) ->
    Src = <<
        "-module(breath_rop_).\n "
        "-compile({parse_transform, breath_rop}).\n "
        "f(Int) -> \n "
        "    case breath:rop(Int, f2(_)) of\n"
        "        {ok, 2} ->\n "
        "            breath:rop(2, f3(_));\n "
        "        {error, _}=Err ->\n "
        "            Err\n "
        "    end.\n "
        "f2(1) -> {ok, 2};\n "
        "f2(_) -> {error, foo}.\n "
        "f3(X) -> {ok, X*2}.\n "

          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_rop_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({ok, _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_rop}, return_errors, {outdir, Dir}])),
    ?assertMatch({ok, 4}, breath_rop_:f(1)),
    ?assertMatch({error, foo}, breath_rop_:f(2)),
    _ = code:del_path(Dir),
    _ = code:purge(breath_rop_),
    true = code:delete(breath_rop_),
    _ = file:delete(File),
    ok.



'8'(Cfg) ->
    Src = <<
        "-module(breath_rop_).\n "
        "-compile({parse_transform, breath_rop}).\n "
        "f(Int) -> \n "
        "    breath:rop(\n"
        "               Int\n "
        "              ,f2(_)\n "
        "              ,f2(_)\n "
        "              ).\n "
        "f2(1) -> ok.\n "
          >>,
    Dir = ?config(data_dir, Cfg),
    _ = code:add_patha(Dir),
    File = filename:join([Dir, "breath_rop_.erl"]),
    ?assertMatch(ok, file:write_file(File, Src)),
    ?assertMatch({ok, _}, compile:file(File, [verbose, nowarn_export_all, export_all, {parse_transform, breath_rop}, return_errors, {outdir, Dir}])),
    ?assertError({case_clause, ok}, breath_rop_:f(1)),
    _ =
        try
            breath_rop_:f(1)
        catch
            _:{case_clause, ok}:Stacktrace ->
                ?assertMatch([{breath_rop_, f, 1, [_, {line, 6}]}|_], Stacktrace)
        end,

    ?assertError(function_clause, breath_rop_:f(2)),
    _ =
        try
            breath_rop_:f(2)
        catch
            _:function_clause:Stacktrace2 ->
                ?assertMatch([{breath_rop_, f2, [2], [_, {line, 9}]}|_], Stacktrace2)
        end,

    _ = code:del_path(Dir),
    _ = code:purge(breath_rop_),
    true = code:delete(breath_rop_),
    _ = file:delete(File),
    ok.


'9'(_) ->
    ?assertMatch([_|_], breath_rop:format_error(argument)),
    ?assertMatch([_|_], breath_rop:format_error(arguments)),
    ok.