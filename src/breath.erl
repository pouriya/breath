%%% ----------------------------------------------------------------------------
%%% @author   <pouriya.jahanbakhsh@gmail.com>
%%% @version
%%% @doc
%%%           Brought a breath of fresh air to the Erlang coding.<br/>
%%% @end

%% -----------------------------------------------------------------------------
-module(breath).
-author("pouriya.jahanbakhsh@gmail.com").
%% -----------------------------------------------------------------------------
%% Exports:

%% 'compile' export:
-export([parse_transform/2]).

%% -----------------------------------------------------------------------------
%% 'compile' callback:

%% @hidden
parse_transform(AST, Opts) ->
    DebugFlag =
        case os:getenv("ERL_BREATH_DEBUG") of
            DebugEnv when DebugEnv == "1" orelse DebugEnv == "true" ->
                io:format("*DBG* breath: Original AST is:\n~tp\n", [AST]),
                true;
            _ ->
                false
        end,
    ParseTransformFun =
        fun(Callback, OldAST) ->
            NewAST = Callback:parse_transform(OldAST, Opts),
            _ = DebugFlag andalso io:format(
                "*DBG*  breath: ~tp:parse_transform/2 yields:\n~tp\n",
                [Callback, AST]
            ),
            NewAST
        end,
    lists:foldl(ParseTransformFun, AST, callbacks()).

%% -------------------------------------------------------------------------------------------------
%% Internals:

callbacks() ->
    [breath_rop, breath_pipeline].
