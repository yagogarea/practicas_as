-module(effects).

% Public API
-export([print/1, even_print/1]).

%%--------------------------------------------------------------------
%% @doc Print function.
%% @spec print(N :: pos_integer()) -> ok
%% @end
%%--------------------------------------------------------------------
print(N) when is_integer(N) andalso N > 0 ->
    print(N, 1).

%%--------------------------------------------------------------------
%% @doc Even print function.
%% @spec even_print(N :: pos_integer()) -> ok
%% @end
%%--------------------------------------------------------------------
even_print(N) when is_integer(N) andalso N > 0 ->
    even_print(N, 2).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
print(0, _) ->
    ok;
print(N, Acc)->
    io:format("~w", [Acc]),
    print(N - 1, Acc + 1).

even_print(N, _) when N =< 2 ->
    ok;
even_print(N, Acc) ->
    io:format("~w", [Acc]),
    even_print(N - 2, Acc + 2).
