%%%-------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%% @copyright (C) 2019 Duncan Paul Attard
%%% @version 0.9
%%%
%%% @doc
%%%
%%% @end
%%% Created: 23. Sep 2019
%%% 
%%% Copyright (c) 2019 Duncan Paul Attard <duncanatt@gmail.com>
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------
-module(async_mon).
-author("Duncan Paul Attard").
-version("0.9").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").
%%-include("util.hrl").

%%% Public API exports.
-export([start/3, start_online/3, start_offline/3, stop/0]).

%%% Internal exports.
-export([init_online/2, init_offline/1, loop_online/5, loop_offline/1]).



-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------


-define(STATE_WAITING, waiting).
-define(STATE_JOINING, joining).
-define(STATE_EXITING, exiting).

%%-define(MODE_DIRECT, direct).
%%-define(MODE_PRIORITY, priority).


%%-ifdef(TEST).
%%-define(MON_INFO_ETS_NAME, mon_state).
%%-define(MON_INFO_INV_ETS_NAME, mon_inv_state).
%%-endif.

%%-record(mon_stats, {
%%  cnt_spawn = 0 :: non_neg_integer(),
%%  cnt_exit = 0 :: non_neg_integer(),
%%  cnt_send = 0 :: non_neg_integer(),
%%  cnt_receive = 0 :: non_neg_integer(),
%%  cnt_other = 0 :: non_neg_integer()
%%}).
%%
%%-record(mon_state, {
%%  routes = #{} :: routes_table(),
%%  group = #{} :: group(),
%%  monitors = fun({_, _, _}) -> undefined end :: monitors(),
%%  trace = [] :: list(),
%%  stats = #mon_stats{} :: mon_stats()
%%}).


%%% ----------------------------------------------------------------------------
%%% Type declarations.
%%% ----------------------------------------------------------------------------

-type shutdown_state() :: joining | waiting | exiting.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------


-spec start(Mfa, MfaSpec, Opts) -> Root :: pid()
  when
  Mfa :: mfa(),
  MfaSpec :: analyzer:mfa_spec(),
  Opts :: proplists:proplist().
start({Mod, Fun, Args}, MfaSpec, Opts) when is_function(MfaSpec, 1) ->

  % Start tracing framework.
  trace_lib:start(evm),

  % Launch system in a blocked state, and wait for ack from launching process.
  % Syn is sent prior the system starts executing in order to prevent the
  % potential loss of trace events.
  Self = self(),
  Pid = spawn(fun() -> util:syn(Self), apply(Mod, Fun, Args) end),

  % Start root tracer for system. System is currently blocked: this prevents
  % trace events from being lost.
  Merged = proplists:get_value(merged, Opts, false),

  % Launch tracer using separate or merged monitors.
  Root =
    if Merged =:= false -> tracer:start(Pid, MfaSpec, external, self);
%%      true -> tracer_monitor:start(Pid, MfaSpec, self)
      true -> tracer:start(Pid, MfaSpec, internal, self)
    end,

  % Ack root monitor and system, now that the former has been fully started.
  util:syn_ack(Root),
  util:syn_ack(Pid),

  Root.

-spec start_online(Mfa, MfaSpec, Opts) -> Root :: pid()
  when
  Mfa :: mfa(),
  MfaSpec :: analyzer:mfa_spec(),
  Opts :: proplists:proplist().
start_online({Mod, Fun, Args}, MfaSpec, Opts) when is_function(MfaSpec, 1) ->

  % Set up controller monitor. The controller collects all the tracer statistics
  % and monitor verdicts at their time of exit by relying process linking. It
  % also acts as a barrier that waits for *all* tracers, monitors and in
  % particular, the ROOT tracer to finish their execution. This, it does so that
  % monitoring will not be prematurely stopped.
  Self = self(),
  NumMonitors = proplists:get_value(num_monitors, Opts, 0),
  register(?MODULE, Controller = spawn(?MODULE, init_online, [Self, NumMonitors])),

  % Wait for controller to start up and ack before continuing.
  util:syn(Controller),

  % Start tracing framework.
  trace_lib:start(evm),

  % Launch system in a blocked state, and wait for ack from launching process.
  % Syn is sent prior the system starts executing in order to prevent the
  % potential loss of trace events.
  Pid = spawn(fun() -> util:syn(Self), apply(Mod, Fun, Args) end),

  % Start root tracer for system. System is currently blocked: this prevents
  % trace events from being lost.
  Root = tracer:start(Pid, MfaSpec, Controller),

  % Ack root monitor and system, now that the former has been fully started.
  util:syn_ack(Root),
  util:syn_ack(Pid),

  % Wait for controller to terminate and ack before continuing. This ensures
  % that the monitoring system never terminates before all monitors have exited
  % either by reaching a verdict or because the process or group of processes
  % they were monitoring have themselves exited. Blocking in this manner is also
  % necessary when the system is run from the command line using '-s init stop'
  % which expects the whole system (including the root tracer) not to exit
  % unless its execution has been completed.
  util:syn(Controller),

  % Stop.
  stop(),
  io:format("~w ~w unblocked by collector monitor.~n", [?MODULE, self()]),
  Root.


-spec start_offline(File, Pid, MfaSpec) -> Root :: pid()
  when
  File :: string(),
  Pid :: pid(),
  MfaSpec :: analyzer:mfa_spec().
start_offline(File, Pid, MfaSpec) when is_function(MfaSpec, 1) ->

  % Set up controller monitor. The controller collects all the tracer statistics
  % and monitor verdicts at their time of exit by relying process linking.
  Self = self(),
  register(?MODULE, Controller = spawn(?MODULE, init_offline, [Self])),

  % Wait for controller to start up and ack before continuing.
  util:syn(Controller),

  % Start tracing framework.
  trace_lib:start({log, File}),

  % Start root tracer for system. Since this is offline monitoring, the trace
  % is assumed to exist. As in the online case, the root tracer is bootstrapped
  % by specifying the root process of the system (obtained from the trace log).
  Root = tracer:start(Pid, MfaSpec, external, Controller),

  % Ack root monitor.
  util:syn_ack(Root),
  Root.

-spec stop() -> ok | {error, not_started}.
stop() ->
%%  case whereis(?MODULE) of undefined -> ok; _ -> unregister(?MODULE) end,
  case catch unregister(?MODULE) of
    true ->
      ok;
    {'EXIT', _} ->
      io:format("Unable to unregister ~s; already terminated.~n", [?MODULE])
  end,
  tracer:stop(),
  trace_lib:stop().


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------


% If the number of monitors specified is less than the actual number, then the
% statistics are only partially collected, but the system does not block.
-spec init_online(Owner, NumMonitors) -> no_return()
  when
  Owner :: tracer:parent(),
  NumMonitors :: non_neg_integer().
init_online(Owner, NumMonitors) ->

  util:syn_ack(Owner),
  process_flag(trap_exit, true),

  ?INFO("Expecting ~w monitor(s).", [NumMonitors]),

  % The number of counts to wait before exiting. This essentially works like a
  % counting semaphore, in that when the count reaches 0, loop terminates. The
  % count is two times the number of expected monitors since the collector
  % expects two exits from a monitor: one from the tracer, and one from the
  % monitor itself.
  Counts = NumMonitors * 2,
  io:format("Expecting ~w counts.~n", [Counts]),
  loop_online(?STATE_JOINING, Counts, Owner, tracer:new_mon_stats(), tracer:new_mon_stats()).

%% @private The loop for the online monitoring collector.
%%
%% The loop waits for exit notifications from tracers and monitors, and also
%% calculates the expected and actual trace event stats. Syncing the main
%% process with the collector loop ensures a clean shutdown of both the
%% collector and the simulator and simulation stats collection. Syncing is
%% managed as follows:
%% {@ol
%%   {@item Loop is started in JOINING mode. When in this state, the loop
%%          dequeues tracer and monitor exit notifications from the controller's
%%          mailbox, acting as a counting semaphore that decrements an expected
%%          number of such notifications. It also waits and dequeues one ready
%%          notification from the master: when this is the case, the collector
%%          switches to WAITING mode. The semaphore count is > 0.
%%   }
%%   {@item When in WAITING mode, the controller issues a stop command to the
%%          simulation stats collector to shut it down. It then switches itself
%%          to EXITING mode. The semaphore count == 0.
%%   }
%%   {@item Finally, when in EXITING mode, the controller waits for a final exit
%%          notification from the root tracer. Once this is notification is
%%          dequeued the stats are output and the owner process is ack'ed. The
%%          semaphore count == 0.
%%   }
%% }
-spec loop_online(State, Counts, Owner, Stats, ExpectedStats) -> ok
  when
  State :: shutdown_state(),
  Counts :: non_neg_integer(),
  Owner :: tracer:parent(),
  Stats :: tracer:event_stats(),
  ExpectedStats :: tracer:event_stats().
loop_online(?STATE_EXITING, 0, Owner, Stats, ExpectedStats) ->
  receive
    {'EXIT', Pid, {garbage_collect, {root, _}}} ->
      ?INFO("Received exit notification from ROOT tracer ~w.", [Pid]),
      tracer:show_stats(Stats, ExpectedStats),
      util:syn_ack(Owner),
      ok
  end;
loop_online(?STATE_WAITING, 0, Owner, Stats, ExpectedStats) ->
  ?INFO("Received all exit notifications..waiting for ROOT tracer.."),
  collector:stop(), % TODO: This is hardcoded, I know, but I need it for now!
  loop_online(?STATE_EXITING, 0, Owner, Stats, ExpectedStats);
loop_online(State, Count, Owner, Stats, ExpectedStats)
  when State =:= ?STATE_JOINING; State =:= ?STATE_WAITING ->

  % Make sure that the count does not decrement past 0.
  Count0 = if Count - 1 < 0 -> 0; true -> Count - 1 end,

  receive
    {'$cmd', ready, CntTerm, CntSend, CntRecv} ->
      ?INFO("Notified by ready command from master process."),

      % Controller received notification that master process has exited. The
      % simulation is now complete, but the controller still has to wait for all
      % the tracers and monitors to finish their execution in turn. Compute
      % expected trace event statistics.
      ExpectedStats0 = tracer:new_mon_stats(0, 0, CntSend, CntRecv, 0, CntTerm),
      loop_online(?STATE_WAITING, Count, Owner, Stats, ExpectedStats0);

    {'EXIT', Pid, {garbage_collect, {tracer, Stats0}}} ->
      ?INFO(">>>> Exit notification from tracer ~w with stats ~w; ~b remaining.",
        [Pid, Stats0, Count0]),

      % Aggregate statistics.
      Stats1 =
        if Count > 0 -> tracer:cum_sum_stats(Stats, Stats0); true -> Stats end,

      loop_online(State, Count0, Owner, Stats1, ExpectedStats);

    {'EXIT', Pid, {garbage_collect, {monitor, Verdict}}} ->
      ?INFO("++++ Exit notification from monitor ~w with verdict ~w; ~b remaining.",
        [Pid, Verdict, Count0]),
      loop_online(State, Count0, Owner, Stats, ExpectedStats)
  end.









-spec init_offline(Owner :: tracer:parent()) -> no_return().
init_offline(Owner) ->
  process_flag(trap_exit, true),
  util:syn_ack(Owner),
  loop_offline(Owner).

-spec loop_offline(Owner :: tracer:parent()) -> no_return().
loop_offline(Owner) ->
  receive
    {'EXIT', Pid, {garbage_collect, {tracer, Stats0}}} ->
      ?INFO("Received exit notification from tracer ~w with stats ~w.", [Pid, Stats0]),
      loop_offline(Owner);
    {'EXIT', Pid, {garbage_collect, {monitor, Verdict}}} ->
      ?INFO("Received exit notification from monitor ~w with verdict ~s.", [Pid, Verdict]),
      loop_offline(Owner)
  end.


















