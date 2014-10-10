-module(ms).
-author("albertofem").

%% API
-export([start/1, to_slave/2, init_slaves/2, loop/1]).

init_slaves(N, SlaveCollection) ->
  PID = spawn_link(ms, slave, N),
  init_slaves(N-1, [{N, PID} | SlaveCollection]);
init_slaves(0, SlaveCollection) ->
  process_flag(trap_exit, true),
  loop(SlaveCollection).

start(N) ->
  PID = spawn(ms, init_slaves, N),
  register(master, PID).

to_slave(Message, N) ->
  ok.

loop(SlaveCollection) ->
  receive
    stop -> stop(SlaveCollection);
    {'EXIT', _, N} ->
        SlavePID = spawn_link(ms, slave, [N]),
        io:format("Restarting new slave"),
        master(lists:keyreplace(N, 1, SlaveCollection, {N, SlavePID}));
    {N, Message} ->
        io:format("Sending message to slave"),
      ok
  end.