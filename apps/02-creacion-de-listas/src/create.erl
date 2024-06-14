-module(create).

% Public API
-export([create/1, reverse_create/1]).

%%--------------------------------------------------------------------
%% @doc Create function.
%% @spec create(Value :: pos_integer()) -> [pos_integer()]
%% @end
%%--------------------------------------------------------------------
create(N) ->
    create(N, []).

%%--------------------------------------------------------------------
%% @doc Reverse create function.
%% @spec reverse_create(Value :: pos_integer()) -> [pos_integer()]
%% @end
%%--------------------------------------------------------------------
reverse_create(N) ->
    List = create(N),
    RevList = rev_list(List, []),
    RevList.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------

create(0, List) ->
    List;
create(N, List) ->
    create(N-1, [N | List]).

rev_list([], Acc) ->
    Acc; 
rev_list([H | T], Acc) ->
    rev_list(T, [H | Acc]).

