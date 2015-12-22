-module(gen_e).

%% An experimental module for testing port behaviours, in particular
%% detecting that a port goes down or are taken down.

%% Uses the c file e.c for stand alone program driver and e_li.c for
%% the linked in version.

% Gen server version

-behaviour(gen_server).

%% API
-export([start/0, start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Function API
-export([set/1, get/0, inc/1, crash/0, exit/0, exit/1, undef/0]).

%% Help things
-export([flush/0, pid/0, port/0]).


-define(SERVER, ?MODULE).

-define(TIMEOUT, 1000).

-record(state, {port, from}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    start([]).

start(Args) ->
    Type = proplists:get_value(type, Args, standalone),
    {ok, File} =
        case Type of
            standalone ->
                {ok, "./e.out"};
            linkedin ->
                F = "e_li",
                case erl_ddll:load_driver(".", F) of
                    ok ->
                        io:format("Driver loaded~n"),
                        {ok, F};
                    {error, already_loaded} ->
                        io:format("Driver already loaded~n"),
                        {ok, F};
                    {error, Reason} ->
                        io:format("Could not load driver (~p)~n", [Reason]),
                        {error, could_not_load_driver}
                end
        end,
    case gen_server:start({local, ?SERVER}, ?MODULE, [{file,File} | Args], []) of
        Reply = {ok, _} ->
            monitor(process, ?SERVER),
            Reply;
        Error ->
            Error
    end.

stop() ->
    flush(),
    Res = (catch gen_server:call(?SERVER, stop)),
    io:format("~p: Res = ~p~n", [self(), Res]),
    timer:sleep(?TIMEOUT),
    flush(),
    ok.


%%%===================================================================
%%% Port API
%%%===================================================================

set(Value) ->
    command({set, Value}).

get() ->
    command(get).

inc(Value) ->
    command({inc, Value}).

crash() ->
    command(crash).

exit() ->
    ?MODULE:exit(0).

exit(Value) ->
    command({exit, Value}).

undef() ->
    command(undef).

%% ----

command(C) ->
    flush(),
    Res = (catch gen_server:call(?SERVER, {command, C})),
    io:format("~p: Res = ~p~n", [self(), Res]),
    timer:sleep(?TIMEOUT),
    flush(),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
    TrapExit = proplists:get_value(trap_exit, Args, true),
    Params = proplists:get_value(params, Args, [exit_status]),
    Type = proplists:get_value(type, Args, standalone),
    File = proplists:get_value(file, Args, should_never_happen),

    io:format("TrapExit = ~p~n", [TrapExit]),
    io:format("Params = ~p~n", [Params]),
    io:format("Type = ~p~n", [Type]),
    io:format("File = ~p~n", [File]),

    process_flag(trap_exit, TrapExit),
    Port = open_port({spawn, File}, [{packet, 1}] ++ Params),
    {ok, #state{port = Port}}.

handle_call({command,Command}, From, State=#state{port = Port}) ->
    Port ! {self(), {command, encode(Command)}},
    {noreply, State#state{from = From}, ?TIMEOUT};
handle_call(stop, _From, State=#state{port = Port}) ->
    Port ! {self(), close},
    {noreply, State, ?TIMEOUT};
handle_call(port, _From, State=#state{port = Port}) ->
    {reply, Port, State};
handle_call(Request, _From, State) ->
    io:format("~p: Got unexpected call: ~p~n", [self(), Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("~p: Got unexpected cast: ~p~n", [self(), Msg]),
    {noreply, State}.

handle_info({_, {data, Data}}, State=#state{from = From}) ->
    gen_server:reply(From, decode(Data)),
    {noreply, State#state{from = undefined}};
handle_info(timeout, State) ->
    io:format("~p: Timeout, giving up~n", [self()]),
    {stop, normal, State};
%% handle_info({'EXIT',Port,Reason}, State) when is_port(Port) ->
%%     io:format("Got port EXIT, reason: ~p~n", [Reason]),
%%     {noreply, State, ?TIMEOUT};
%% handle_info({'DOWN',Ref,port,Port,Reason}, State) when is_reference(Ref), is_port(Port) ->
%%     io:format("Got port DOWN, reason: ~p~n", [Reason]),
%%     {noreply, State, ?TIMEOUT};
%% handle_info({Port,Info}, State) when is_port(Port) ->
%%     io:format("Got port down info: ~p~n", [Info]),
%%     {noreply, State, ?TIMEOUT};
handle_info(Info, State) ->
    io:format("~p: Got some info: ~p~n", [self(), Info]),
    {noreply, State, ?TIMEOUT}.

terminate(Reason, _State) ->
    io:format("~p: The port handler terminates with reason: ~p~n",
              [self(), Reason]),
    timer:sleep(?TIMEOUT),
    flush(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

flush() ->
    flush(1).

flush(N) ->
    receive
        X ->
            io:format("~p: flush(~p) = ~p~n", [self(), N,X]),
            flush(N+1)
    after
        0 ->
            ok
    end.

pid() ->
    whereis(?SERVER).

port() ->
    catch gen_server:call(?SERVER, port).

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
