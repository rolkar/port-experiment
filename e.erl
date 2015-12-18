-module(e).

%% An experimental module for testing port behaviours, in particular
%% detecting that a port goes down or are taken down.

%% Uses the c file e.c for stand alone program driver


%% Start/stop API
-export([start/0, stop/0]).

%% Callbacks
-export([init/1]).

%% Functional API
-export([set/1, get/0, inc/1, crash/0, exit/0, exit/1, undef/0]).

%% Help functions
-export([flush/0, port/0]).


-define(TIMEOUT, 10000).

start() ->
    start("./e.out").

start(ExtPrg) ->
    Pid = spawn(?MODULE, init, [ExtPrg]),
    monitor(process, Pid),
    Pid.

stop() ->
    flush(),
    try
        ?MODULE ! stop,
        receive
            X ->
                io:format("Stop: Message: ~p~n", [X])
        after
            ?TIMEOUT ->
                io:format("Stop: Timeout~n", [])
        end
    catch
        T:M ->
            io:format("Stop: ~p~n", [{T,M}])
    end.

set(Value) ->
    call_port({set, Value}).

get() ->
    call_port(get).

inc(Value) ->
    call_port({inc, Value}).

crash() ->
    call_port(crash).

exit() ->
    ?MODULE:exit(0).

exit(Val) ->
    call_port({exit,Val}).

undef() ->
    call_port(undef).

init(ExtProg) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtProg}, [{packet, 1}]),
    loop(Port).


%% Private stuff

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            try
                Port ! {self(), {command, encode(Msg)}}
            catch
                T:M ->
                    io:format("Port command: failed: ~p~n", [{T,M}]),
                    erlang:exit(port_terminated)
            end,
            receive
                {Port, {data, Data}} ->
                    Caller ! {?MODULE, decode(Data)};
                X ->
                    io:format("Port command: Message: ~p~n", [X]),
                    erlang:exit(port_terminated)
            after
                ?TIMEOUT ->
                    io:format("Port command: Timeout~n", []),
                    erlang:exit(port_terminating)
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    erlang:exit(normal);
                X ->
                    io:format("Port close: Message: ~p~n", [X]),
                    erlang:exit(port_terminated)
            after
                ?TIMEOUT ->
                    io:format("Port close: Timeout~n", []),
                    erlang:exit(port_terminating)
            end;
        {port, Caller} ->
            Caller ! {port, Port},
            loop(Port);
        X ->
            io:format("Port loop: Message: ~p~n", [X]),
            erlang:exit(port_terminated)
    end.

encode({set, Val}) ->
    [1, Val];
encode(get) ->
    [2];
encode({inc, Val}) ->
    [3, Val];
encode(crash) ->
    [4];
encode({exit,Val}) ->
    [5, Val];
encode(undef) ->
    [6].

decode([Int]) ->
    Int.

call_port(Msg) ->
    flush(),
    try
        ?MODULE ! {call, self(), Msg},
        receive
            {?MODULE, Result} ->
                Result;
            X ->
                io:format("Call: Message: ~p~n", [X])
        after
            ?TIMEOUT ->
                io:format("Call: Timeout~n", [])
        end
    catch
        T:M ->
            io:format("Call: ~p~n", [{T,M}])
    end.

flush() ->
    flush(1).

flush(N) ->
    receive
        X ->
            io:format("~p: ~p~n", [N,X]),
            flush(N+1)
    after
        0 ->
            ok
    end.

port() ->
    flush(),
    try
        ?MODULE ! {port, self()} ,
        receive
            {port, Port} ->
                Port;
            X ->
                io:format("Port: Message: ~p~n", [X])
        after
            ?TIMEOUT ->
                io:format("Port: Timeout~n", [])
        end
    catch
        T:M ->
            io:format("Port: ~p~n", [{T,M}])
    end.
