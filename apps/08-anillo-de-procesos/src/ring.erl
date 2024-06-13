-module(ring).

% Public API
-export([start/3]).
-export([init/4, loop/2]).

%%--------------------------------------------------------------------
%% @doc Start function.
%% @spec start(ProcNum :: pos_integer(), MsgNum :: pos_integer(), Message :: term()) -> ok
%% @end
%%--------------------------------------------------------------------
start(ProcNum, MsgNum, Message) ->
    register(master,spawn(?MODULE, init, [ProcNum, MsgNum, Message, self()])),
    receive
        ring_close ->
            ok
    end.
    
init(0, _MsgNum, _Message, _From)->
    ok;
init(1, MsgNum, Message, From) ->
    master ! {MsgNum - 1, Message},
    loop(master, From);
init(ProcNum, MsgNum, Message, From) ->
    Next = spawn(?MODULE, init, [ProcNum - 1, MsgNum, Message, From]),
    loop(Next, From).

loop(finish, From) ->
    receive
        stop ->
            From ! ring_close
    end;
loop(Next, From) ->
    receive 
        {0, Message} ->
            io:format("~p~n", [Message]),
            Next ! stop,
            loop(finish, From);
        {MsgNum, Message} ->
            io:format("~p~n", [Message]),
            Next ! {MsgNum - 1, Message},
            loop(Next, From);
        stop ->
            Next ! stop
    end.
