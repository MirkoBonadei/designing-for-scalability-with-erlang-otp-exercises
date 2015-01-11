-module(frequency).
-behaviour(gen_server).

-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type frequency() :: pos_integer().
-type allocated_frequency() :: {frequency(), pid()}.
-type allocation_message() :: {allocate, pid()}.
-type deallocation_message() :: {deallocate, frequency()}.
-type stop_message() :: stop.
-type allocation_reply() :: {ok, frequency()} | {error, no_frequency}.
-type deallocation_reply() :: ok.
-type termination_reply() :: ok.

-type loop_data() :: {[frequency()], [allocated_frequency()]}.

%% server API
-spec start() -> ignore | {error, _} | {ok, pid()}.
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> termination_reply().
stop() -> 
    gen_server:cast(?MODULE, stop).

-spec allocate() -> allocation_reply().
allocate() -> 
    gen_server:call(?MODULE, {allocate, self()}).

-spec deallocate(frequency()) -> deallocation_reply().
deallocate(Freq) -> 
    gen_server:cast(?MODULE, {deallocate, Freq}).

%% gen_server callbacks
-spec init(_) -> {ok, loop_data()}.
init(_Args) ->
    {ok, {get_frequencies(), []}}.

-spec handle_call(allocation_message(), {pid(), _}, loop_data()) -> 
                         {reply, {error, no_frequency} | {ok, frequency()}, loop_data()}.
handle_call({allocate, Pid}, _From, Frequencies) ->
    {NewFrequencies, Reply} = allocate(Frequencies, Pid),
    {reply, Reply, NewFrequencies}.

-spec handle_cast(deallocation_message() | stop_message(), loop_data()) ->
                         {noreply, loop_data()} | {stop, normal, loop_data()}.
handle_cast({deallocate, Freq}, Frequencies) ->
    NewFrequencies = deallocate(Frequencies, Freq),
    {noreply, NewFrequencies};
handle_cast(stop, Frequencies) ->
    {stop, normal, Frequencies}.

-spec handle_info(term(), loop_data()) -> {noreply, loop_data()}.
handle_info(_Message, LoopData) ->
    {noreply, LoopData}.

-spec terminate(atom(), loop_data()) -> term().
terminate(_Reason, _Frequencies) -> 
    ok.

%% internal functions
-spec get_frequencies() -> [frequency(), ...].
get_frequencies() -> [10, 11, 12, 13, 14, 15].

-spec allocate(loop_data(), pid()) -> {loop_data(), allocation_reply()}.
allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

-spec deallocate(loop_data(), frequency()) -> loop_data().
deallocate({Free, Allocated}, Freq) ->
    NewAllocated = lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.
