%%% -*- coding: utf-8 -*-
-module(boolean).

% Public API
-export([b_not/1, b_and/2, b_or/2]).

%%--------------------------------------------------------------------
%% @doc Operator 'not'.
%% @spec b_not(Value :: boolean()) -> boolean()
%% @end
%%--------------------------------------------------------------------
b_not(false) ->
    true;
b_not(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Operator 'and'.
%% @spec b_and(Value1 :: boolean(),
%%             Value2 :: boolean()) -> boolean()
%% @end
%%--------------------------------------------------------------------

b_and(true,true) ->
    true;
b_and(_,_) ->
    false.
%%--------------------------------------------------------------------
%% @doc Operator 'or'.
%% @spec b_or(Value1 :: boolean(),
%%            Value2 :: boolean()) -> boolean()
%% @end
%%--------------------------------------------------------------------

b_or(true, _) ->
    true;
b_or(_, true) ->
    true;
b_or(_, _) ->
    false.