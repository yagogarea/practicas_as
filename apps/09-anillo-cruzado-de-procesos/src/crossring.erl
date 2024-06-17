-module(crossring).

% Public API
-export([start/3]).
-export([init/5, init_cross/4, loop/3, loop_cross/3, rec_start/1]).

%%--------------------------------------------------------------------
%% @doc Start function.
%% @spec start(ProcNum :: pos_integer(), MsgNum :: pos_integer(), Message :: term()) -> ok
%% @end
%%--------------------------------------------------------------------

start(ProcNum, MsgNum, Message) ->
    register(cross, spawn(?MODULE, init_cross, [ProcNum - 1, MsgNum, Message, self()])),
    wait_procs(ProcNum).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------

init_cross(0, MsgNum, Message, From) -> 
    self() ! {MsgNum - 1, Message, none},
    io:format("~p~n", [Message]),
    loop_cross(self(), none, From);
init_cross(ProcNum, MsgNum, Message, From) ->
    P1 = ProcNum div 2,
    P2 = ProcNum - P1,
    create_rings(P1, P2, MsgNum, Message, From).

create_rings(0, 1, MsgNum, Message, From) -> 
    Next = spawn(?MODULE, init, [0, MsgNum, Message, self(), From]),
    rec_start(1),
    Next ! {MsgNum - 1, Message, one},
    io:format("~p~n", [Message]),
    loop_cross(Next, one, From);
create_rings(P1, P2, MsgNum, Message, From) ->
    Next = spawn(?MODULE, init, [P1 - 1, MsgNum, Message, self(), From]),
    Next2 = spawn(?MODULE, init, [P2 - 1, MsgNum, Message, self(), From]),
    rec_start(2),
    Next ! {MsgNum - 1, Message, first},
    io:format("~p~n", [Message]),
    loop_cross(Next, Next2, From).

init(0, _MsgNum, _Message, Cross, From) ->
    Cross ! readyone,
    loop(Cross, Cross, From);
init(ProcNum, MsgNum, Message, Cross, From) ->
    Next = spawn(?MODULE, init, [ProcNum - 1, MsgNum, Message, Cross, From]), 
    loop(Next, Cross, From).

loop_cross(Cross, none, From) -> % 1 proc
    receive
        {0, _Message, _State} ->
            Cross ! stop,
            loop_cross(Cross, none, From);
        {MsgNum, Message, State} ->
            Cross ! {MsgNum - 1, Message, State},
            io:format("~p~n", [Message]),
            loop_cross(Cross, none, From);
        stop ->
            From ! close
    end;
loop_cross(Next, one, From) -> % 2 proc
    receive
        {0, _Message, _State} ->
            self() ! fin,
            loop_cross(Next, one, From);
        {MsgNum, Message, State} ->
            Next ! {MsgNum - 1, Message, State},
            io:format("~p~n", [Message]),
            loop_cross(Next, one, From);
        fin ->
            Next ! stop,
            From ! close
    end;
loop_cross(Next1, Next2, From) ->
    receive
        {0, _Message, _State} ->
            self() ! fin,
            loop_cross(Next1, Next2, From);
        {MsgNum, Message, first} ->
            Next1 ! {MsgNum - 1, Message, second},
            io:format("~p~n", [Message]),
            loop_cross(Next1, Next2, From);
        {MsgNum, Message, second} ->
            Next2 ! {MsgNum - 1, Message, first},
            io:format("~p~n", [Message]),
            loop_cross(Next1, Next2, From);
        fin ->
            Next1 ! stop,
            Next2 ! stop,
            From ! close
        end.

loop(Cross, Cross, From) ->
    receive
        {0, _Message, _State} ->
            Cross ! fin,
            loop(Cross, Cross, From);
        {MsgNum, Message, State} ->
            Cross ! {MsgNum - 1, Message, State},
            io:format("~p~n", [Message]),
            loop(Cross, Cross, From);
        stop ->
            Cross ! stop,
            From ! close
    end;
loop(Next, Cross, From) ->
    receive
        {0, _Message, _State} ->
            Cross ! fin,
            loop(Next, Cross, From);
        {MsgNum, Message, State} ->
            Next ! {MsgNum - 1, Message, State},
            io:format("~p~n", [Message]),
            loop(Next, Cross, From);
        stop ->
            Next ! stop,
            From ! close
    end.

rec_start(0) ->
    ok;
rec_start(Nrec) ->
    receive
        readyone ->
            rec_start(Nrec - 1)
    end.

wait_procs(0) ->
    ok;
wait_procs(ProcNum) ->
    receive
        close ->
            wait_procs(ProcNum - 1)
    end.
