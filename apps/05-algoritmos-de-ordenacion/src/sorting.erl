-module(sorting).

% Public API
-export([quicksort/1, mergesort/1]).

%%--------------------------------------------------------------------
%% @doc Quicksort function.
%% @spec quicksort(List :: [integer()]) -> [integer()]
%% @end
%%--------------------------------------------------------------------
quicksort([]) ->
    [];
quicksort([X]) ->
    [X];
quicksort([H | T])  ->
   List1 = [X || X <- T, X =< H],
   List2 = [X || X <- T, X > H],
   quicksort(List1) ++ [H] ++  quicksort(List2).

%%--------------------------------------------------------------------
%% @doc Mergesort function.
%% @spec mergesort(List :: [integer()]) -> [integer()]
%% @end
%%--------------------------------------------------------------------
mergesort([]) ->
    [];
mergesort([X]) ->
    [X];
mergesort([X1, X2]) ->
    quicksort([X1, X2]);
mergesort(List) when is_list(List) ->
    {List1, List2} = lists:split(lsize(List) div 2, List),
    quicksort(mergesort(List1) ++ mergesort(List2)).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
lsize([]) ->
    0;
lsize([_|T]) ->
    1 + lsize(T).
