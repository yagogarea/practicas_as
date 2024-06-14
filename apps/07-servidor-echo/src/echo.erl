-module(echo).

% Public API
-export([start/0, stop/0, print/1]).
-export([loop/0]).

%%--------------------------------------------------------------------
%% @doc Start function.
%% @spec start() -> ok
%% @end
%%--------------------------------------------------------------------
start() ->
    register(?MODULE, spawn(?MODULE, loop, [])),
    ok.

%%--------------------------------------------------------------------
%% @doc Stop function.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    ?MODULE ! {stop, self()},
    receive
        stopped ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Print function.
%% @spec print(Term :: term()) -> ok
%% @end
%%--------------------------------------------------------------------
print(Term) ->
    ?MODULE ! {print, Term},
    ok.


loop() ->
    receive
        {print, Term} ->
            io:format("~p~n",[Term]),
            loop();
        {stop, From} ->
            From ! stopped;
        _ ->
            erlang:throw("error")
    end.

