-module(db).

% Public API
-export([new/0, write/3, delete/2, read/2, match/2, destroy/1]).

%%--------------------------------------------------------------------
%% @doc New function.
%% @spec new() -> term()
%% @end
%%--------------------------------------------------------------------
new() ->
    [].

%%--------------------------------------------------------------------
%% @doc Write function.
%% @spec write(Key :: integer, Element :: term(), DbRef :: term()) -> term()
%% @end
%%--------------------------------------------------------------------
write(Key, Element, DbRef) when is_list(DbRef) ->
    [{Key, Element} | DbRef];
write(_, _, _) ->
    erlang:throw("error").

%%--------------------------------------------------------------------
%% @doc Delete function.
%% @spec delete(Key :: integer, DbRef :: term()) -> term()
%% @end
%%--------------------------------------------------------------------
delete(_, []) ->
    [];
delete(Key, [{Key, _} | T]) ->
    T;
delete(Key, [{X, Y} | T]) ->
    [{X, Y}| delete(Key, T)];
delete(_Key, _DbRef) ->
    erlang:throw("error").

%%--------------------------------------------------------------------
%% @doc Read function.
%% @spec read(Key :: integer(), DbRef :: term()) -> {ok, Element}
%% | {error, instance}
%% @end
%%--------------------------------------------------------------------
read(_, []) ->
    {error, instance};
read(Key, [{Key, Element} | _]) ->
    {ok, Element};
read(Key, [_ | T]) ->
    read(Key, T).

%%--------------------------------------------------------------------
%% @doc Match function.
%% @spec match(Element :: term(), DbRef :: term()) -> [integer()]
%% @end
%%--------------------------------------------------------------------
match(Element, DbRef) when is_list(DbRef) ->
    [X || {X, E} <- DbRef, E =:= Element];
match(_Key, _DbRef) ->
    erlang:throw("error").

%%--------------------------------------------------------------------
%% @doc Destroy function.
%% @spec destroy(DbRef :: term()) -> ok
%% @end
%%--------------------------------------------------------------------
destroy(DbRef) when is_list(DbRef)->
    [];
destroy(_DbRef) ->
    erlang:throw("error").
