-module(frequency).
-author("albertofem").

-behaviour(gen_server).

-export([start_link/0, stop/0, allocate/0, deallocate/1, ping/1, add_frequency/1]).
-export([init/1, terminate/2, handle_cast/2, handle_call/3]).

-define(SERVER, ?MODULE).
-define(PING_INTERVAL, 1000). % 1 second
-define(PING_TIMEOUT, 5000). % 5 seconds

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

allocate() ->
  gen_server:call(?MODULE, {allocate, self()}).

deallocate(Freq) ->
  gen_server:call(?MODULE, {deallocate, Freq}).

add_frequency(NewFrequency) ->
  gen_server:cast(?MODULE, {add_frequencies, NewFrequency}).

ping(Freq) ->
  gen_server:cast(?MODULE, {ping, Freq}).

handle_call({allocate, Pid}, _From, Frequencies) ->
  {NewFrequencies, Reply} = allocate(Frequencies, Pid),
  {reply, Reply, NewFrequencies};
handle_call({deallocate, Freq}, _From, Frequencies) ->
  NewFrequencies = deallocate(Frequencies, Freq),
  {reply, ok, NewFrequencies}.
% handle_call({ping, Freq}, From, Frequencies) ->
  % refresh ping data to 0

handle_cast(stop, Frequencies) ->
  {stop, normal, Frequencies};
handle_cast({add_frequencies, NewFrequency}, {Frequencies, Allocated}) ->
  {noreply, {Frequencies++[NewFrequency], Allocated}}.
% handle_cast({ping, Freq}, Frequencies) ->


init(_Args) ->
  %erlang:send_after(?PING_INTERVAL, self(), ping_refresh),
  %erlang:send_after(?PING_TIMEOUT, self(), ping_check),
  {ok, {get_frequencies(), []}}.

terminate(_Reason, _Frequencies) ->
  ok.

get_frequencies() -> [10, 11, 12, 13, 14, 15].

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequencies}};
allocate({[Freq|Frequencies], Allocated}, Pid) ->
  {{Frequencies,[{Freq,Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  {value, {Freq, _Pid}} = lists:keysearch(Freq, 1, Allocated),
  NewAllocated = lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free], NewAllocated}.

%% handle_info(ping_refresh, _Frequencies) ->
%%   io:write("Received info to refresh pings"),
%%   erlang:send_after(?PING_INTERVAL, self(), ping_refresh),
%%   {noreply, ok};
%% handle_info(ping_check, _Frequencies) ->
%%   io:write("Received info to check pings"),
%%   erlang:send_after(?PING_TIMEOUT, self(), ping_check),
%%   {noreply, ok}.