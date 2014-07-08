-module(frequency).
-author("albertofem").

-behaviour(gen_server).

-export([
  start_link/1,
  stop/0,
  state/0,
  allocate/0,
  deallocate/0,
  ping/0,
  add_frequency/1
]).

-export([
  init/1,
  terminate/2,
  handle_cast/2,
  handle_call/3,
  handle_info/2
]).

-define(SERVER, ?MODULE).
-define(PING_INTERVAL, 1000). % 1 second
-define(PING_TIMEOUT, 50000). % 5 seconds

-record(frequencies, {
  free,
  allocated
}).

%%& FREQUENCY API %%%

start_link(Frequencies) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Frequencies, []).

stop() ->
  gen_server:cast(?MODULE, stop).

allocate() ->
  gen_server:call(?SERVER, allocate).

state() ->
  gen_server:call(?SERVER, state).

deallocate() ->
  gen_server:call(?SERVER, deallocate).

ping() ->
  gen_server:call(?SERVER, ping).

add_frequency(NewFrequency) ->
  gen_server:cast(?SERVER, {add_frequency, NewFrequency}).

%%% GEN SERVER CALLBACKS %%%

%%% SYNC CALLS %%%

handle_call(allocate, {Pid, _}, Frequencies) ->
  {NewFrequencies, Reply} = allocate(Frequencies, Pid),
  {reply, Reply, NewFrequencies};
handle_call(deallocate, {Pid, _}, Frequencies) ->
  NewFrequencies = deallocate(Frequencies, Pid),
  {reply, ok, NewFrequencies};
handle_call(ping, {Pid, _}, Frequencies) ->
  NewFrequencies = ping(Frequencies, Pid),
  {reply, ok, NewFrequencies};
handle_call(state, _, Frequencies) ->
  io:write(Frequencies),
  {reply, ok, Frequencies}.

handle_cast(stop, Frequencies) ->
  {stop, normal, Frequencies};
handle_cast({add_frequency, NewFrequency}, #frequencies{free = Free, allocated = Allocated}) ->
  NewFrequencies = #frequencies{
    free = Free++[NewFrequency],
    allocated = Allocated
  },
  {noreply, NewFrequencies}.

handle_info(ping_refresh, #frequencies{free = Free, allocated = []}) ->
  NewFrequencies = #frequencies{
      free = Free,
      allocated = []
  },
  {noreply, NewFrequencies};
handle_info(ping_refresh, #frequencies{free = Free, allocated = [{Freq, Pid, Ping} | Allocated]}) when Ping < ?PING_TIMEOUT ->
  NewFrequencies = #frequencies{
      free = Free,
      allocated = [{Freq, Pid, Ping+?PING_INTERVAL}] ++ Allocated
  },
  {noreply, NewFrequencies};
handle_info(ping_refresh, #frequencies{free = Free, allocated = [{Freq, _, Ping} | Allocated]}) when Ping >= ?PING_TIMEOUT ->
  NewFrequencies = #frequencies{
      free = Free ++ [Freq],
      allocated = Allocated
  },
  {noreply, NewFrequencies}.

init(Frequencies) ->
  timer:send_interval(?PING_INTERVAL, ping_refresh),
  NewFrequencies = #frequencies{
    free = Frequencies,
    allocated = []
  },
  {ok, NewFrequencies}.

terminate(_Reason, _Frequencies) ->
  ok.

allocate(#frequencies{free = [], allocated = Allocated}, _Pid) ->
  NewFrequencies = #frequencies{
      free = [],
      allocated = Allocated
  },
  {NewFrequencies, {error, no_frequencies}};
allocate(#frequencies{free = [Freq | Free], allocated = Allocated}, Pid) ->
  NewFrequencies = #frequencies{
      free = Free,
      allocated = Allocated++[{Freq, Pid, 0}]
  },
  {NewFrequencies, {ok, Freq}}.

deallocate(#frequencies{free = Free, allocated = [{Freq, Pid, _} | Allocated]}, Pid) ->
  NewFrequencies = #frequencies{
      free = Free ++ [Freq],
      allocated = Allocated
  },
  NewFrequencies.

ping(#frequencies{free = Free, allocated = [{Freq, Pid, _} | Allocated]}, Pid) ->
  NewFrequencies = #frequencies{
      free = Free,
      allocated = [{Freq, Pid, 0}]++Allocated
  },
  NewFrequencies.