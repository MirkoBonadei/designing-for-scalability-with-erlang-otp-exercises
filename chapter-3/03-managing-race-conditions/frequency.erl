-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/1, handle/2, terminate/1]).

-type frequency() :: pos_integer().
-type allocated_frequency() :: {frequency(), pid()}.

%% COOL: since we are hiding the 'stop' message in the generic server we
%%       can declare the type 'message' without leaving out the 'stop' message 
%%       as I have done before. A good way to be explicit on the protocol between 
%%       the API functions and the 'handle/2'.
-type message() :: {allocate, pid()} | {deallocate, frequency()}.

%% ???: this time I have split out the kind of reply we can give.
%%      I think that this is cool to make the protocol clear between the 
%%      module API functions and the second element of the return tuple 
%%      of the 'handle/2' function. Do you agree?
-type allocation_reply() :: {ok, frequency()} | {error, no_frequency}.
-type deallocation_reply() :: ok.
-type termination_reply() :: ok.

-type loop_data() :: {[frequency()], [allocated_frequency()]}.

%% TODO: Does it makes sense to define a spec for functions that only 
%% delegates to the generic module?
%% In this case the return value is the return value of the 'erlang:register/2'
%% function and it is not specific.
start() ->
    server:start(frequency, []).

-spec init(any()) -> loop_data().
init(_Args) ->
    {get_frequencies(), []}.

-spec get_frequencies() -> [frequency(), ...].
get_frequencies() -> [10, 11, 12, 13, 14, 15].

-spec terminate(loop_data()) -> termination_reply().
terminate(_Frequencies) -> 
    ok.

-spec stop() -> termination_reply().
stop() -> server:stop(frequency).

-spec allocate() -> allocation_reply().
allocate() -> server:call(frequency, {allocate, self()}).

-spec deallocate(frequency()) -> deallocation_reply().
deallocate(Freq) -> server:call(frequency, {deallocate, Freq}).

-spec handle(message(), loop_data()) -> 
                    {loop_data(), allocation_reply()} |
                    {loop_data(), deallocation_reply()}.
handle({allocate, Pid}, Frequencies) ->
    allocate(Frequencies, Pid);
handle({deallocate, Freq}, Frequencies) ->
    deallocate(Frequencies, Freq).

-spec allocate(loop_data(), pid()) -> {loop_data(), allocation_reply()}.
allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

-spec deallocate(loop_data(), frequency()) -> {loop_data(), deallocation_reply()}.
deallocate({Free, Allocated}, Freq) ->
    NewAllocated = lists:keydelete(Freq, 1, Allocated),
    {{[Freq|Free], NewAllocated}, ok}.
