% Auto-generated by https://github.com/Pouriya-Jahanbakhsh/estuff

-module(user_default).
-export([c/0, r/0, cr/0, cl/0, test/0]).


c() ->
    io:format(os:cmd("make shell-compile")).


r() ->
    % If you have Erlang source files not just in "src", modify erlang_source_files/0
    Modules  = [module(X) || X <- erlang_source_files()],
    io:format("Reloading modules ...\n\n"),
    Reload =
        fun(Mod, Acc) ->
            reload(Mod),
            _ = 
                if
                    Acc rem 3 == 0 ->
                        io:nl();
                    true ->
                        ok
                end,
            Acc + 1
        end,
    Count = lists:foldl(Reload, 1, lists:sort(Modules)),
    _ =
        if
            Count rem 3 == 2 ->
                io:nl();
            true ->
                ok
        end,
    _ = io:nl(),
    io:format("~p Modules have been reloaded\n\n", [Count-1]).

cr() ->
    c(),
    r().


cl() ->
    io:format("\033[2J\033[u").


test() ->
    io:format("Running ./test/shell_quick_test.script\n"),
    Result = file:script("./test/shell_quick_test.script"),
    io:nl(),
    Result.


erlang_source_files() ->
    SrcFiles = filelib:wildcard("**/*.erl", "_build"),
    lists:filter(fun is_erlang_source_file/1, SrcFiles).


is_erlang_source_file(File) ->
    case filename:extension(File) of
        ".erl" ->
            true;
        _ ->
            false
    end.


module(File) ->
    % remove ".erl" from the end of file:
     erlang:list_to_atom(lists:reverse(lists:reverse(filename:basename(File)) -- "lre.")).


reload(Mod) ->
    io:format("~-24.24s", [Mod]),
    code:soft_purge(Mod),
    code:load_file(Mod).
