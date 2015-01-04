%% generic server module which abstracts all the generic behaviours of 
%% a server, which are:
%% - start the server
%% - stop the server
%% - message passing
%% - looping

%% TODO: I have put specifications also in this module because even if there 
%%       are lots of 'any()' we can be strict on the 'Mod'/'Name' parameter 
%%       which must be an atom() or better, the name of a module.

-module(server).
-export([start/2, stop/1, call/2]).
-export([init/2]).

-spec start(module(), any()) -> true.
start(Name, Args) ->
    register(Name, spawn(server, init, [Name, Args])).

-spec init(module(), any()) -> any().
init(Mod, Args) ->
    State = Mod:init(Args),
    loop(Mod, State).

-spec stop(module()) -> any().
stop(Name) ->
    Name ! {stop, self()},
    receive
        {reply, Reply} ->
            Reply
    end.

-spec call(module(), any()) -> any().
call(Name, Msg) ->
    Name ! {request, self(), Msg},
    receive
        {reply, Reply} ->
            Reply
    end.

-spec reply(pid(), any()) -> any().
reply(To, Reply) ->
    To ! {reply, Reply}.

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
