%% Code from
%% Erlang Programming
%% Francecso Cesarini and Simon Thompson
%% O'Reilly, 2008
%% http://oreilly.com/catalog/9780596518189/
%% http://www.erlangprogramming.org/
%% (c) Francesco Cesarini and Simon Thompson
-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

-type frequency() :: pos_integer().
-type allocated_frequency() :: {frequency(), pid()}.
-type message() :: allocate | {deallocate, frequency()} | stop.
-type allocation_reply() :: {ok, frequency()} | {error, no_frequency}.
-type reply() :: allocation_reply() | ok.
-type loop_data() :: {[frequency()], [allocated_frequency()]}.

%% These are the start functions used to create and
%% initialize the server.

-spec start() -> true.
start() ->
    register(frequency, spawn(frequency, init, [])).

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

-spec get_frequencies() -> [frequency(), ...].
get_frequencies() -> [10, 11, 12, 13, 14, 15].

%% The client Functions
-spec stop() -> ok.
stop() -> call(stop).

-spec allocate() -> allocation_reply().
allocate() -> call(allocate).

-spec deallocate(frequency()) -> ok.
deallocate(Freq) -> call({deallocate, Freq}).

%% We hide all message passing and the message
%% protocol in a functional interface.
-spec call(message()) -> reply().
call(Message) ->
    frequency ! {request, self(), Message},
    receive
        {reply, Reply} -> Reply
    end.

%% The Main Loop
-spec loop(loop_data()) -> {reply, ok}.
loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid , {deallocate, Freq}} ->
            NewFrequencies = deallocate(Frequencies, Freq),
            reply(Pid, ok),
            loop(NewFrequencies);
        {request, Pid, stop} ->
            reply(Pid, ok)
    end.

-spec reply(pid(), reply()) -> {reply, reply()}.
reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.
-spec allocate(loop_data(), pid()) -> {loop_data(), allocation_reply()}.
allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

-spec deallocate(loop_data(), frequency()) -> loop_data().
deallocate({Free, Allocated}, Freq) ->
    NewAllocated = lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.

%% ===================================================================
%% Example of usage
%%
%% 1> c(frequency).
%% {ok,frequency}
%% 2> frequency:start().
%% true
%% 3> frequency:allocate().
%% {ok,10}
%% 4> frequency:allocate().
%% {ok,11}
%% 5> frequency:allocate().
%% {ok,12}
%% 6> frequency:allocate().
%% {ok,13}
%% 7> frequency:allocate().
%% {ok,14}
%% 8> frequency:allocate().
%% {ok,15}
%% 9> frequency:allocate().
%% {error,no_frequency}
%% 10> frequency:deallocate(11).
%% ok
%% 11> frequency:allocate().
%% {ok,11}
%% 12> frequency:stop().
%% ok
