-module(crossring).

% Public API
-export([start/3]).
-export([init/5, init/6, loop/4]).

%%--------------------------------------------------------------------
%% @doc Start function.
%% @spec start(ProcNum :: pos_integer(), MsgNum :: pos_integer(), Message :: term()) -> ok
%% @end
%%--------------------------------------------------------------------

start(ProcNum, MsgNum, Message) ->
    Middle = (ProcNum - 1) div 2,
    register(master, spawn(?MODULE, init, [ProcNum - 1, Middle, MsgNum, Message, self()])),
    wait(ProcNum).

wait(0) ->
    ok;
wait(ProcNum) ->
    receive
        close ->
            wait(ProcNum - 1)
    end.

init(ProcNum, Middle, MsgNum, Message, From) ->
    Master = whereis(master),
    init(ProcNum, Middle, MsgNum, Message, From, Master).

init(0, Middle, MsgNum, Message, From, Master) when is_pid(Middle) ->
    io:format("~p  ~n", [Message]),
    master ! {MsgNum - 1, Message, schild},
    loop(self(), Master, From, Master);

init(0, _Middle, MsgNum, Message, From, Master) ->
    io:format("~p ~n", [Message]),
    master ! {MsgNum - 1, Message, self()},
    loop(self(), Master, From, Master);

init(ProcNum, ProcNum, MsgNum, Message, From, Master) ->
    register(schild, spawn(?MODULE, init, [ProcNum - 1, self(), MsgNum, Message, From, Master])),
    loop(self(), Master, From, Master);

init(ProcNum, Middle, MsgNum, Message, From, Master) ->
    Next = spawn(?MODULE, init, [ProcNum - 1, Middle, MsgNum, Message, From, Master]),
    loop(self(), Next, From, Master).


loop(Master, Next, From, Master) ->
    receive 
        {0, _Message, Schild} ->
            Master ! {close, Schild},
            loop(Master, Next, From, Master);
        {MsgNum, Message, Schild} ->
            io:format("~p ~n", [Message]),
            Next ! {MsgNum - 1, Message, Next},
            loop(Master, Schild, From, Master);
        {close, Schild} ->
            Next ! stop,
            case Schild of
                Master ->
                    ok;
                Next ->
                    ok;
                _ ->
                    Schild ! stop
            end,
            From ! close
    end;
loop(Me, Master, From, Master) ->
    receive 
        {0, _Message, Schild} ->
            master ! {close, Schild},
            loop(Me, Master, From, Master);
        {MsgNum, Message, Schild} ->
            io:format("~p ~n", [Message]),
            Master ! {MsgNum - 1, Message, Schild},
            loop(Me, Master, From, Master);
        stop ->
            From ! close
    end;
loop(Me, Next, From, Master) ->
    receive 
        {0, _Message, Schild} ->
            master ! {close, Schild},
            loop(Me, Next, From, Master);
        {MsgNum, Message, Schild} ->
            io:format("~p ~n", [Message]),
            Next ! {MsgNum - 1, Message, Schild},
            loop(Me, Next, From, Master);
        stop ->
            Next ! stop,
            From ! close
    end.