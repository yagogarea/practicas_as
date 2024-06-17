-module(manipulating).

% Public API
-export([filter/2, reverse/1, concatenate/1, flatten/1]).

%%--------------------------------------------------------------------
%% @doc Filter function.
%% @spec filter(List :: [integer()], N :: integer()) -> [integer()]
%% @end
%%--------------------------------------------------------------------
filter(List, N) when is_integer(N) ->
    filter(List, N, []);
filter(_List, _N) ->
    erlang:throw("NAN").

%%--------------------------------------------------------------------
%% @doc Reverse function.
%% @spec reverse(List :: [integer()]) -> [integer()]
%% @end
%%--------------------------------------------------------------------
reverse(List) ->
    reverse(List, []).

%%--------------------------------------------------------------------
%% @doc Concatenate function.
%% @spec concatenate(ListOfLists :: [[integer()]]) -> [integer()]
%% @end
%%--------------------------------------------------------------------
concatenate(List)->
    Result = concatenate(List, []),
    reverse(Result).
        
%%--------------------------------------------------------------------
%% @doc Flatten function.
%% @spec flatten(DeepList :: [integer() | list()]) -> [integer()]
%% @end
%%--------------------------------------------------------------------

flatten(List) ->
    Result = flatten(List, []),
    reverse(Result).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------

aux([], Acc) ->
    Acc;
aux([H | T], Acc) ->
    aux(T, [H | Acc]).


concatenate([], Acc) ->
    Acc;
concatenate([H | T], Acc) when is_list(H)->
    concatenate(T, aux(H, Acc));
concatenate(_ListOfLists, _) ->
    erlang:throw("Not a list of lists").


filter([], _N, Acc) ->
    reverse(Acc);
filter([H | T], N, Acc) when H > N ->
    filter(T, N, Acc);
filter([H | T], N, Acc) ->
    filter(T, N, [H | Acc]).


flatten([], Acc) ->
    Acc;
flatten([H | T], Acc) when is_list(H)->
    flatten(T, flatten(H, Acc));
flatten([H | T], Acc) ->
    flatten(T, [H | Acc]);
flatten(_List, _) ->
    erlang:throw("Not a list").


reverse([], Acc) ->
    Acc;
reverse([H | T], Acc) ->
   reverse(T, [H | Acc]);
reverse(_, _) ->
    erlang:throw("Not a list").
