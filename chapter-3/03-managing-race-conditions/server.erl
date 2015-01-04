%% Improved `call/3` to:
%% - be sure that the reply comes from the right process
%% - avoid to be blocked for more that 5 seconds by default (configurable by the user
%%   through the `call/3` function
%% - catch errors in case we are sending a message (with !) to a non registered process
%% - be notified in case the server is DOWN and we are waiting for a respose

-module(server).
-export([start/2, stop/1, call/2, call/3]).
-export([init/2]).

-spec start(module(), any()) -> true.
start(Name, Args) ->
    register(Name, spawn(server, init, [Name, Args])).

-spec init(module(), any()) -> any().
init(Mod, Args) ->
    State = Mod:init(Args),
    loop(Mod, State).

-spec stop(module()) -> any().
%% TODO: remove duplication with `call/3` function.
%%       I can't remove it right away since Ref is part of the message to send
%%       and because of this I have to keep `erlang:monitor` duplicated.
%%       I have finished my time for today but I am thinking at a solution.
stop(Name) ->
    Ref = erlang:monitor(process, Name),
    catch Name ! {stop, {Ref, self()}},
    receive
        {reply, Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, _Name, _Reason} ->
            {error, no_proc}
    after 5000 ->
            exit(timeout)
    end.

-spec call(module(), any()) -> any().
call(Name, Msg) ->
    call(Name, Msg, 5000).

-spec call(module(), any(), pos_integer()) -> any().
call(Name, Msg, Timeout) ->
    Ref = erlang:monitor(process, Name),
    catch Name ! {request, {Ref, self()}, Msg},
    receive
        {reply, Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, _Name, _Reason} ->
            {error, no_proc}
    after Timeout ->
            exit(timeout)
    end.    

-spec reply(pid(), any()) -> any().
reply({Ref, To}, Reply) ->
    To ! {reply, Ref, Reply}.

-spec loop(module(), any()) -> any().
loop(Mod, State) ->
    receive
        {request, From, Msg} ->
            {NewState, Reply} = Mod:handle(Msg, State),
            reply(From, Reply),
            loop(Mod, NewState);
        {stop, From} ->
            Reply = Mod:terminate(State),
            reply(From, Reply)
    end.
