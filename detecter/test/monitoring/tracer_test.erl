%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Tracer tests.
%%%
%%% @end
%%% 
%%% Copyright (c) 2021, Duncan Paul Attard <duncanatt@gmail.com>
%%%
%%% This program is free software: you can redistribute it and/or modify it 
%%% under the terms of the GNU General Public License as published by the Free 
%%% Software Foundation, either version 3 of the License, or (at your option) 
%%% any later version.
%%%
%%% This program is distributed in the hope that it will be useful, but WITHOUT 
%%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
%%% FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
%%% more details.
%%%
%%% You should have received a copy of the GNU General Public License along with 
%%% this program. If not, see <https://www.gnu.org/licenses/>.
%%% ----------------------------------------------------------------------------
-module(tracer_test).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").


%%-export([init/0]).

%% TODO: These unit tests are time dependent, which is not the way to go when to
%% TODO: test. This was done to try and affect the trace event routing algorithm
%% TODO: as much as possible, since one cannot use messages to influence the
%% TODO: different interleaved of the offline tracer (responsible for only the
%% TODO: system process interleaving via the order of trace events) together
%% TODO: with the trace event routing algorithm, which is itself concurrent. The
%% TODO: problem with these tests is that they are too fine-grained, and attempt
%% TODO: check every possible execution step, rather than the final monitored
%% TODO: system configuration. The assertions used by the test must therefore
%% TODO: be rewritten to check the monitored system state in a coarser manner.
%% TODO: I suspect this must be done once at the end of each test run. For this
%% TODO: reason, these batch of test will be excluded for the main build until
%% TODO: these are properly rewritten.


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Mock PIDs of system processes being monitored.
-define(P1, c:pid(0, 11, 0)).
-define(P2, c:pid(0, 13, 0)).
-define(P3, c:pid(0, 17, 0)).
-define(P4, c:pid(0, 19, 0)).
-define(P5, c:pid(0, 23, 0)).

%% Sleeping times that generates pauses in tests. This is used to enable tests
%% to wait until certain trace processing finishes. Timing, though frowned upon
%% in concurrency, is useful here to avoid complicating the tests tracer module
%% with added synchronization directives.
-define(small_wait, timer:sleep(800)).
%%-define(big_wait, timer:sleep(1400)).


%%% ----------------------------------------------------------------------------
%%% Tests.
%%% ----------------------------------------------------------------------------

%%init() ->
%%  process_flag(trap_exit, true),
%%  loop().
%%
%%loop() ->
%%
%%  receive
%%    {'EXIT', Pid, {garbage_collect, {tracer, Stats0}}} ->
%%      tracer:show_stats(Stats0, Pid, "tracer");
%%    {'EXIT', Pid, {garbage_collect, {root, Stats0}}} ->
%%      tracer:show_stats(Stats0, Pid, "ROOT")
%%  end,
%%  loop().

problem_solve_test_() -> {"Problem solve example to test timeouts in test.
                           Delete once the proper tests are resolved",
  {foreachx,
    fun({Trace, Monitors}) ->

%%      Controller = spawn(?MODULE, init, []),

      % Start the monitoring system in offline mode. The system under scrutiny
      % is not run; rather a run simulation is performed by posting trace events
      % to the underlying log tracer.
      monitor:start_offline("", ?P1, Monitors, []),
      log_tracer:post_events(Trace)
    end,
    fun(_, _) ->
      % Stop monitoring system and underlying tracer.
      monitor:stop()
    end,
    [
      {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
        {delay, 0, {exit, ?P1, normal}},
        {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
        {delay, 0, {fork, ?P3, ?P4, {m4, f4, []}}}],
        fun({m2, f2, []}) ->
          {ok, fun(_) -> 'end' end};
          ({m3, f3, []}) ->
            {ok, fun(_) -> 'end' end};
          ({m4, f4, []}) ->
            {ok, fun(_) -> 'end' end};
          ({_, _, _}) ->
            undefined
        end
      },
        fun(_, _) ->
          {"All processes have dedicated monitors",
%%            ?_test(
            {timeout, 5, ?_test(
            begin

            % Wait until monitoring simulation completes and traces are
            % collected.
              ?small_wait,
%%              timer:sleep(6000),

              % Monitor M1 should be only attached to P1. It should also be
              % terminated since P1 terminates.
              {M1, _G1, T1} = tracer:get_mon_info_rev(?P1),
              ?assertNot(is_process_alive(M1)),

              % Monitor M2 should be only attached to P2. It should also be
              % alive.
              {M2, G2, T2} = tracer:get_mon_info_rev(?P2),
              ?assertEqual([?P2], G2),
              ?assert(is_process_alive(M2)),

              % Monitor M3 should be only attached to P3. It should also be
              % alive.
              {M3, G3, T3} = tracer:get_mon_info_rev(?P3),
              ?assertEqual([?P3], G3),
              ?assert(is_process_alive(M3)),

              % Monitor M4 should be only attached to P4. It should also be
              % alive.
              {M4, G4, T4} = tracer:get_mon_info_rev(?P4),
              ?assertEqual([?P4], G4),
              ?assert(is_process_alive(M4)),

              % Monitor M1 receives trace events for P1 only.
              ?assertEqual([
                {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                {trace, ?P1, exit, normal}
              ], T1),

              % Monitor M2 receives trace events for P2 only.
              ?assertEqual([
                {trace, ?P2, spawn, ?P3, {m3, f3, []}}
              ], T2),

              % Monitor M3 receives trace events for P3 only.
              ?assertEqual([
                {trace, ?P3, spawn, ?P4, {m4, f4, []}}
              ], T3),

              % Monitor M4 has no events.
              ?assertEqual([], T4),

              % Post receive events and exit events to terminate P2, P3, P4.
              log_tracer:post_events([
                {delay, 0, {recv, ?P2, msg_to_p2}},
                {delay, 0, {recv, ?P3, msg_to_p3}},
                {delay, 0, {recv, ?P4, msg_to_p4}},
                {delay, 0, {exit, ?P2, normal}},
                {delay, 0, {exit, ?P3, normal}},
                {delay, 0, {exit, ?P4, normal}}
              ]),
              ?small_wait,

              % Monitor M2 receives trace events for P2 only.
              {_, _, T2_1} = tracer:get_mon_info(M2),
              ?assertEqual([
                {trace, ?P2, 'receive', msg_to_p2},
                {trace, ?P2, exit, normal}
              ], T2_1 -- T2),

              % Monitor M3 receives trace events for P3 only.
              {_, _, T3_1} = tracer:get_mon_info(M3),
              ?assertEqual([
                {trace, ?P3, 'receive', msg_to_p3},
                {trace, ?P3, exit, normal}
              ], T3_1 -- T3),

              % Monitor M4 receives trace events for P4 only.
              {_, _, T4_1} = tracer:get_mon_info(M4),
              ?assertEqual([
                {trace, ?P4, 'receive', msg_to_p4},
                {trace, ?P4, exit, normal}
              ], T4_1 -- T4),

              % All monitors should be terminated.
              ?assertNot(is_process_alive(M2)),
              ?assertNot(is_process_alive(M3)),
              ?assertNot(is_process_alive(M4))
            end
%%        end
          )
          }}
        end}
    ]}}.


generic_monitor_test_for_testing_to_remove_test_() -> {"Generic dynamic monitor routing algorithm test",
  {foreachx,
    fun({Trace, Monitors}) ->

%%      Controller = spawn(?MODULE, init, []),

      % Start the monitoring system in offline mode. The system under scrutiny
      % is not run; rather a run simulation is performed by posting trace events
      % to the underlying log tracer.
      monitor:start_offline("", ?P1, Monitors, []),
      log_tracer:post_events(Trace)
    end,
    fun(_, _) ->
      % Stop monitoring system and underlying tracer.
      monitor:stop()
    end,
    [
      {{
        [{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
          {delay, 0, {send, ?P2, ?P2, msg_to_p2_1}},
          {delay, 0, {recv, ?P2, msg_to_p2_1}},
          {delay, 0, {exit, ?P1, normal}},
          {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
          {delay, 0, {recv, ?P2, msg_to_p2_2}},
          {delay, 0, {exit, ?P2, normal}},
          {delay, 0, {send, ?P3, ?P2, msg_to_p2_2}},
          {delay, 0, {exit, ?P3, normal}}],
        fun({m2, f2, []}) ->
          {ok, fun(_) -> 'end' end};
          ({m3, f3, []}) ->
            {ok, fun(_) -> 'end' end};
          ({_, _, _}) ->
            undefined
        end
      },
        fun(_, _) ->
          {"All processes have dedicated monitors (messages sent on start up)",
            ?_test(
              begin

              % Wait until monitoring simulation completes and traces are
              % collected.
                ?small_wait,

                % Monitor M1 receives trace events for P1 only.
                {_, _, T1} = tracer:get_mon_info_rev(?P1),
                ?assertEqual([
                  {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                  {trace, ?P1, exit, normal}
                ], T1),

                % Monitor M2 receives trace events for P2 only.
                {_, _, T2} = tracer:get_mon_info_rev(?P2),
                ?assertEqual([
                  {trace, ?P2, send, msg_to_p2_1, ?P2},
                  {trace, ?P2, 'receive', msg_to_p2_1},
                  {trace, ?P2, spawn, ?P3, {m3, f3, []}},
                  {trace, ?P2, 'receive', msg_to_p2_2},
                  {trace, ?P2, exit, normal}
                ], T2),

                % Monitor M3 receives trace events for P3 only.
                {_, _, T3} = tracer:get_mon_info_rev(?P3),
                ?assertEqual([
                  {trace, ?P3, send, msg_to_p2_2, ?P2},
                  {trace, ?P3, exit, normal}
                ], T3)
              end
            )}
        end}

%%      {{[
%%
%%      ],
%%        fun({_, _, _}) ->
%%          false
%%        end
%%      },
%%        fun(_, _) ->
%%          {"Tests that the Monitor M1 is shared by all processes", ?_test(
%%            begin
%%
%%            % Wait until monitoring simulation completes and traces are
%%            % collected.
%%              ?small_wait
%%
%%            end
%%          )}
%%        end}

    ]}
}.

%% @doc Tests the generic dynamic routing logic and the assignment of tracers to
%% system processes.
generic_monitor_test_() -> {"Generic dynamic monitor routing algorithm test",
  {foreachx,
    fun({Trace, Monitors}) ->

      % Start the monitoring system in offline mode. The system under scrutiny
      % is not run; rather a run simulation is performed by posting trace events
      % to the underlying log tracer.
      monitor:start_offline("", ?P1, Monitors, []),
      log_tracer:post_events(Trace)
    end,
    fun(_, _) ->

      % Stop monitoring system and underlying tracer.
      monitor:stop()
    end,
    [
      {{
        [{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
          {delay, 0, {send, ?P2, ?P2, msg_to_p2_1}},
          {delay, 0, {recv, ?P2, msg_to_p2_1}},
          {delay, 0, {exit, ?P1, normal}},
          {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
          {delay, 0, {recv, ?P2, msg_to_p2_2}},
          {delay, 0, {exit, ?P2, normal}},
          {delay, 0, {send, ?P3, ?P2, msg_to_p2_2}},
          {delay, 0, {exit, ?P3, normal}}],
        fun({m2, f2, []}) ->
          {ok, fun(_) -> 'end' end};
          ({m3, f3, []}) ->
            {ok, fun(_) -> 'end' end};
          ({_, _, _}) ->
            undefined
        end
      },
        fun(_, _) ->
          {"All processes have dedicated monitors (messages sent on start up)",
            ?_test(
              begin

              % Wait until monitoring simulation completes and traces are
              % collected.
                ?small_wait,

                % Monitor M1 receives trace events for P1 only.
                {_, _, T1} = tracer:get_mon_info_rev(?P1),
                ?assertEqual([
                  {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                  {trace, ?P1, exit, normal}
                ], T1),

                % Monitor M2 receives trace events for P2 only.
                {_, _, T2} = tracer:get_mon_info_rev(?P2),
                ?assertEqual([
                  {trace, ?P2, send, msg_to_p2_1, ?P2},
                  {trace, ?P2, 'receive', msg_to_p2_1},
                  {trace, ?P2, spawn, ?P3, {m3, f3, []}},
                  {trace, ?P2, 'receive', msg_to_p2_2},
                  {trace, ?P2, exit, normal}
                ], T2),

                % Monitor M3 receives trace events for P3 only.
                {_, _, T3} = tracer:get_mon_info_rev(?P3),
                ?assertEqual([
                  {trace, ?P3, send, msg_to_p2_2, ?P2},
                  {trace, ?P3, exit, normal}
                ], T3)
              end
            )}
        end},
      {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
        {delay, 0, {exit, ?P1, normal}},
        {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
        {delay, 0, {fork, ?P3, ?P4, {m4, f4, []}}}],
        fun({_, _, _}) ->
          undefined
        end
      },
        fun(_, _) ->
          {"Tests that the Monitor M1 is shared by all processes", ?_test(
            begin

            % Wait until monitoring simulation completes and traces are
            % collected.
              ?small_wait,

              % Monitor M1 should be alive since it is attached to P1, P2, P3,
              % and P4.
              M1 = tracer:get_proc_mon(?P1),
              ?assert(is_process_alive(M1)),

              % Monitor M1 receives all events.
              {M1, _, T1} = tracer:get_mon_info_rev(?P1),
              ?assertEqual([
                {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                {trace, ?P1, exit, normal},
                {trace, ?P2, spawn, ?P3, {m3, f3, []}},
                {trace, ?P3, spawn, ?P4, {m4, f4, []}}
              ], T1),

              % Post receive events.
              log_tracer:post_events([
                {delay, 0, {recv, ?P2, msg_to_p2}},
                {delay, 0, {recv, ?P3, msg_to_p3}},
                {delay, 0, {recv, ?P4, msg_to_p4}}
              ]),
              ?small_wait,

              % Monitor M1 receives all events.
              {M1, _, T1_1} = tracer:get_mon_info_rev(?P1),
              ?assertEqual([
                {trace, ?P2, 'receive', msg_to_p2},
                {trace, ?P3, 'receive', msg_to_p3},
                {trace, ?P4, 'receive', msg_to_p4}
              ], T1_1 -- T1),

              % Post exit events to terminate P2, P3, P4.
              log_tracer:post_events([
                {delay, 0, {exit, ?P2, normal}},
                {delay, 0, {exit, ?P3, normal}},
                {delay, 0, {exit, ?P4, normal}}
              ]),
              ?small_wait,

              % Monitor M1 should be terminated.
              ?assertNot(is_process_alive(M1))
            end
          )}
        end},
      {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
        {delay, 0, {exit, ?P1, normal}},
        {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
        {delay, 0, {fork, ?P3, ?P4, {m4, f4, []}}}],
        fun({m2, f2, []}) ->
          {ok, fun(_) -> 'end' end};
          ({m4, f4, []}) ->
            {ok, fun(_) -> 'end' end};
          ({_, _, _}) ->
            undefined
        end
      },
        fun(_, _) ->
          {"Monitor M2 is shared by P2 and P3 ERROR", ?_test(
            begin

            % Wait until monitoring simulation completes and traces are
            % collected.
              ?small_wait,
              ?small_wait,

%%              timer:sleep(700),

              % Monitor M1 should be terminated since P1 terminates.
              X = {M1, _G1, T1} = tracer:get_mon_info_rev(?P1),
              ?assertNot(is_process_alive(M1)),

              % Monitor M2 should be shared by P2 and P3. It should also be
              % alive.
              ?small_wait, % Wait for P2 to be detached from M1 and attached to M2.
              ?small_wait,
              A = {M2, G2, T2} = tracer:get_mon_info_rev(?P2),
              io:format(user, "======================== A The tuple is: ~p~n", [A]),
              B = {M3, G3, T3} = tracer:get_mon_info_rev(?P3),
              io:format(user, "======================== B The tuple is: ~p~n", [B]),
              ?assertEqual(M2, M3),
              ?assertEqual(G2, G3), % G = group
              ?assertEqual(T2, T3),
              ?assertEqual([?P2, ?P3], lists:sort(G2)),
              ?assert(is_process_alive(M2)),

              % Monitor M4 should be only attached to P4. It should also be
              % alive.
              ?small_wait, % Wait for the M4 to attach itself to P4.
              ?small_wait,
              X = {M4, G4, T4} = tracer:get_mon_info_rev(?P4),
              io:format(user, "======================== The tuple is: ~p~n", [X]),
              ?assertEqual([?P4], G4),
              ?assert(is_process_alive(M4)),

              % Monitor M1 receives trace events for P1 only.
              ?assertEqual([
                {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                {trace, ?P1, exit, normal}
              ], T1),

              % Monitor M2 receives trace events for P2 and P3.
              ?assertEqual([
                {trace, ?P2, spawn, ?P3, {m3, f3, []}},
                {trace, ?P3, spawn, ?P4, {m4, f4, []}}
              ], T2),

              % Monitor M4 has no events.
              ?assertEqual([], T4),

              % Post receive events and exit events to terminate P2, P3, P4.
              log_tracer:post_events([
                {delay, 0, {recv, ?P2, msg_to_p2}},
                {delay, 0, {recv, ?P3, msg_to_p3}},
                {delay, 0, {recv, ?P4, msg_to_p4}},
                {delay, 0, {exit, ?P2, normal}},
                {delay, 0, {exit, ?P3, normal}},
                {delay, 0, {exit, ?P4, normal}}
              ]),
              ?small_wait,

              % Monitor M2 receives trace events for P2 and P3.
              {_, _, T2_1} = tracer:get_mon_info(M2),
              ?assertEqual([
                {trace, ?P2, 'receive', msg_to_p2},
                {trace, ?P3, 'receive', msg_to_p3},
                {trace, ?P2, exit, normal},
                {trace, ?P3, exit, normal}
              ], T2_1 -- T2),

              % Monitor M4 receives trace events for P4 only.
              {_, _, T4_1} = tracer:get_mon_info(M4),
              ?assertEqual([
                {trace, ?P4, 'receive', msg_to_p4},
                {trace, ?P4, exit, normal}
              ], T4_1),

              % All monitors should be terminated.
              ?assertNot(is_process_alive(M2)),
              ?assertNot(is_process_alive(M4))
            end
          )}
        end},
      {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
        {delay, 0, {exit, ?P1, normal}},
        {delay, 200, {fork, ?P2, ?P3, {m3, f3, []}}},
        {delay, 0, {fork, ?P3, ?P4, {m4, f4, []}}}],
        fun({m2, f2, []}) ->
          {ok, fun(_) -> 'end' end};
          ({m4, f4, []}) ->
            {ok, fun(_) -> 'end' end};
          ({_, _, _}) ->
            undefined
        end
      },
        fun(_, _) ->
          {"Monitor M2 is shared by P2 and P3 (delayed creation of P3)", ?_test(
            begin

            % Wait until monitoring simulation completes and traces are
            % collected.
              ?small_wait,

              % Monitor M1 should be terminated since P1 terminates.
              {M1, _G1, T1} = tracer:get_mon_info_rev(?P1),
              ?assertNot(is_process_alive(M1)),

              % Monitor M2 should be shared by P2 and P3. It should also be
              % alive.
              {M2, G2, T2} = tracer:get_mon_info_rev(?P2),
              {M3, G3, T3} = tracer:get_mon_info_rev(?P3),
              ?assertEqual(M2, M3),
              ?assertEqual(G2, G3),
              ?assertEqual(T2, T3),
              ?assertEqual([?P2, ?P3], lists:sort(G2)),
              ?assert(is_process_alive(M2)),

              % Monitor M4 should be only attached to P4. It should also be
              % alive.
              {M4, G4, T4} = tracer:get_mon_info_rev(?P4),
              ?assertEqual([?P4], G4),
              ?assert(is_process_alive(M4)),

              % Monitor M1 receives trace events for P1 only.
              ?assertEqual([
                {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                {trace, ?P1, exit, normal}
              ], T1),

              % Monitor M2 receives trace events for P2 and P3.
              ?assertEqual([
                {trace, ?P2, spawn, ?P3, {m3, f3, []}},
                {trace, ?P3, spawn, ?P4, {m4, f4, []}}
              ], T2),

              % Monitor M4 has no events.
              ?assertEqual([], T4),

              % Post receive events and exit events to terminate P2, P3, P4.
              log_tracer:post_events([
                {delay, 0, {recv, ?P2, msg_to_p2}},
                {delay, 0, {recv, ?P3, msg_to_p3}},
                {delay, 0, {recv, ?P4, msg_to_p4}},
                {delay, 0, {exit, ?P2, normal}},
                {delay, 0, {exit, ?P3, normal}},
                {delay, 0, {exit, ?P4, normal}}
              ]),
              ?small_wait,

              % Monitor M2 receives trace events for P2 and P3.
              {_, _, T2_1} = tracer:get_mon_info(M2),
              ?assertEqual([
                {trace, ?P2, 'receive', msg_to_p2},
                {trace, ?P3, 'receive', msg_to_p3},
                {trace, ?P2, exit, normal},
                {trace, ?P3, exit, normal}
              ], T2_1 -- T2),

              % Monitor M4 receives trace events for P4 only.
              {_, _, T4_1} = tracer:get_mon_info(M4),
              ?assertEqual([
                {trace, ?P4, 'receive', msg_to_p4},
                {trace, ?P4, exit, normal}
              ], T4_1),

              % All monitors should be terminated.
              ?assertNot(is_process_alive(M2)),
              ?assertNot(is_process_alive(M4))
            end
          )}
        end},
      {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
        {delay, 0, {exit, ?P1, normal}},
        {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
        {delay, 0, {fork, ?P3, ?P4, {m4, f4, []}}}],
        fun({m2, f2, []}) ->
          {ok, fun(_) -> 'end' end};
          ({m3, f3, []}) ->
            {ok, fun(_) -> 'end' end};
          ({m4, f4, []}) ->
            {ok, fun(_) -> 'end' end};
          ({_, _, _}) ->
            undefined
        end
      },
        fun(_, _) ->
          {"All processes have dedicated monitors", ?_test(
            begin

            % Wait until monitoring simulation completes and traces are
            % collected.
              ?small_wait,
%%              timer:sleep(200),

              % Monitor M1 should be only attached to P1. It should also be
              % terminated since P1 terminates.
              {M1, _G1, T1} = tracer:get_mon_info_rev(?P1),
              ?assertNot(is_process_alive(M1)),

              % Monitor M2 should be only attached to P2. It should also be
              % alive.
              {M2, G2, T2} = tracer:get_mon_info_rev(?P2),
              ?assertEqual([?P2], G2),
              ?assert(is_process_alive(M2)),

              % Monitor M3 should be only attached to P3. It should also be
              % alive.
              {M3, G3, T3} = tracer:get_mon_info_rev(?P3),
              ?assertEqual([?P3], G3),
              ?assert(is_process_alive(M3)),

              % Monitor M4 should be only attached to P4. It should also be
              % alive.
              {M4, G4, T4} = tracer:get_mon_info_rev(?P4),
              ?assertEqual([?P4], G4),
              ?assert(is_process_alive(M4)),

              % Monitor M1 receives trace events for P1 only.
              ?assertEqual([
                {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                {trace, ?P1, exit, normal}
              ], T1),

              % Monitor M2 receives trace events for P2 only.
              ?assertEqual([
                {trace, ?P2, spawn, ?P3, {m3, f3, []}}
              ], T2),

              % Monitor M3 receives trace events for P3 only.
              ?assertEqual([
                {trace, ?P3, spawn, ?P4, {m4, f4, []}}
              ], T3),

              % Monitor M4 has no events.
              ?assertEqual([], T4),

              % Post receive events and exit events to terminate P2, P3, P4.
              log_tracer:post_events([
                {delay, 0, {recv, ?P2, msg_to_p2}},
                {delay, 0, {recv, ?P3, msg_to_p3}},
                {delay, 0, {recv, ?P4, msg_to_p4}},
                {delay, 0, {exit, ?P2, normal}},
                {delay, 0, {exit, ?P3, normal}},
                {delay, 0, {exit, ?P4, normal}}
              ]),
              ?small_wait,

              % Monitor M2 receives trace events for P2 only.
              {_, _, T2_1} = tracer:get_mon_info(M2),
              ?assertEqual([
                {trace, ?P2, 'receive', msg_to_p2},
                {trace, ?P2, exit, normal}
              ], T2_1 -- T2),

              % Monitor M3 receives trace events for P3 only.
              {_, _, T3_1} = tracer:get_mon_info(M3),
              ?assertEqual([
                {trace, ?P3, 'receive', msg_to_p3},
                {trace, ?P3, exit, normal}
              ], T3_1 -- T3),

              % Monitor M4 receives trace events for P4 only.
              {_, _, T4_1} = tracer:get_mon_info(M4),
              ?assertEqual([
                {trace, ?P4, 'receive', msg_to_p4},
                {trace, ?P4, exit, normal}
              ], T4_1 -- T4),

              % All monitors should be terminated.
              ?assertNot(is_process_alive(M2)),
              ?assertNot(is_process_alive(M3)),
              ?assertNot(is_process_alive(M4))
            end
          )}
        end},
      {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
        {delay, 0, {exit, ?P1, normal}},
        {delay, 200, {fork, ?P2, ?P3, {m3, f3, []}}},
        {delay, 0, {fork, ?P3, ?P4, {m4, f4, []}}}],
        fun({m2, f2, []}) ->
          {ok, fun(_) -> 'end' end};
          ({m3, f3, []}) ->
            {ok, fun(_) -> 'end' end};
          ({m4, f4, []}) ->
            {ok, fun(_) -> 'end' end};
          ({_, _, _}) ->
            undefined
        end
      },
        fun(_, _) ->
          {"All processes have dedicated monitors (delayed creation of P3)",
            ?_test(
              begin

              % Wait until monitoring simulation completes and traces are
              % collected.
                ?small_wait,

                % Monitor M1 should be only attached to P1. It should also be
                % terminated since P1 terminates.
                {M1, _G1, T1} = tracer:get_mon_info_rev(?P1),
                ?assertNot(is_process_alive(M1)),

                % Monitor M2 should be only attached to P2. It should also be alive.
                {M2, G2, T2} = tracer:get_mon_info_rev(?P2),
                ?assertEqual([?P2], G2),
                ?assert(is_process_alive(M2)),

                % Monitor M3 should be only attached to P3. It should also be alive.
                {M3, G3, T3} = tracer:get_mon_info_rev(?P3),
                ?assertEqual([?P3], G3),
                ?assert(is_process_alive(M3)),

                % Monitor M4 should be only attached to P4. It should also be alive.
                {M4, G4, T4} = tracer:get_mon_info_rev(?P4),
                ?assertEqual([?P4], G4),
                ?assert(is_process_alive(M4)),

                % Monitor M1 receives trace events for P1 only.
                ?assertEqual([
                  {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                  {trace, ?P1, exit, normal}
                ], T1),

                % Monitor M2 receives trace events for P2 only.
                ?assertEqual([
                  {trace, ?P2, spawn, ?P3, {m3, f3, []}}
                ], T2),

                % Monitor M3 receives trace events for P3 only.
                ?assertEqual([
                  {trace, ?P3, spawn, ?P4, {m4, f4, []}}
                ], T3),

                % Monitor M4 has no events.
                ?assertEqual([], T4),

                % Post receive events and exit events to terminate P2, P3, P4.
                log_tracer:post_events([
                  {delay, 0, {recv, ?P2, msg_to_p2}},
                  {delay, 0, {recv, ?P3, msg_to_p3}},
                  {delay, 0, {recv, ?P4, msg_to_p4}},
                  {delay, 0, {exit, ?P2, normal}},
                  {delay, 0, {exit, ?P3, normal}},
                  {delay, 0, {exit, ?P4, normal}}
                ]),
                ?small_wait,

                % Monitor M2 receives trace events for P2 only.
                {_, _, T2_1} = tracer:get_mon_info(M2),
                ?assertEqual([
                  {trace, ?P2, 'receive', msg_to_p2},
                  {trace, ?P2, exit, normal}
                ], T2_1 -- T2),

                % Monitor M3 receives trace events for P3 only.
                {_, _, T3_1} = tracer:get_mon_info(M3),
                ?assertEqual([
                  {trace, ?P3, 'receive', msg_to_p3},
                  {trace, ?P3, exit, normal}
                ], T3_1 -- T3),

                % Monitor M4 receives trace events for P4 only.
                {_, _, T4_1} = tracer:get_mon_info(M4),
                ?assertEqual([
                  {trace, ?P4, 'receive', msg_to_p4},
                  {trace, ?P4, exit, normal}
                ], T4_1 -- T4),

                % All monitors should be terminated.
                ?assertNot(is_process_alive(M2)),
                ?assertNot(is_process_alive(M3)),
                ?assertNot(is_process_alive(M4))
              end
            )}
        end},
      {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
        {delay, 0, {exit, ?P1, normal}},
        {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
        {delay, 0, {fork, ?P3, ?P4, {m4, f4, []}}}],
        fun({m3, f3, []}) ->
          {ok, fun(_) -> 'end' end};
          ({_, _, _}) ->
            undefined
        end
      },
        fun(_, _) ->
          {"Monitor M1 is shared by P1 and P2; M3 by P3 and P4", ?_test(
            begin

            % Wait until monitoring simulation completes and traces are
            % collected.
              ?small_wait,

              % Monitor M1 should be shared by P1 and P2. It should also be
              % alive even though P1 terminates.
              {M1, G1, T1} = tracer:get_mon_info_rev(?P1),
              {M2, G2, T2} = tracer:get_mon_info_rev(?P2),
              ?assertEqual(M1, M2),
              ?assertEqual(G1, G2),
              ?assertEqual(T1, T2),
              ?assertEqual([?P2], G1),
              ?assert(is_process_alive(M1)),

              % Monitor M3 should be shared by P3 and P4. It should also be
              % alive.
              {M3, G3, T3} = tracer:get_mon_info_rev(?P3),
              {M4, G4, T4} = tracer:get_mon_info_rev(?P4),
              ?assertEqual(M3, M4),
              ?assertEqual(G3, G4),
              ?assertEqual(T3, T4),
              ?assertEqual([?P3, ?P4], lists:sort(G3)),
              ?assert(is_process_alive(M3)),

              % Monitor M1 receives trace events for P1 and P2.
              ?assertEqual([
                {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                {trace, ?P1, exit, normal},
                {trace, ?P2, spawn, ?P3, {m3, f3, []}}
              ], T1),

              % Monitor M3 receives trace events for P3 and P4.
              ?assertEqual([
                {trace, ?P3, spawn, ?P4, {m4, f4, []}}
              ], T3),

              % Post receive events and exit events to terminate P2, P3, P4.
              log_tracer:post_events([
                {delay, 0, {recv, ?P2, msg_to_p2}},
                {delay, 0, {recv, ?P3, msg_to_p3}},
                {delay, 0, {recv, ?P4, msg_to_p4}},
                {delay, 0, {exit, ?P2, normal}},
                {delay, 0, {exit, ?P3, normal}},
                {delay, 0, {exit, ?P4, normal}}
              ]),
              ?small_wait,

              % Monitor M1 receives trace events for P2 only as P1 is
              % terminated.
              {_, _, T1_1} = tracer:get_mon_info(M1),
              ?assertEqual([
                {trace, ?P2, 'receive', msg_to_p2},
                {trace, ?P2, exit, normal}
              ], T1_1 -- T1),

              % Monitor M3 receives trace events for P3 and P4.
              {_, _, T3_1} = tracer:get_mon_info(M3),
              ?assertEqual([
                {trace, ?P3, 'receive', msg_to_p3},
                {trace, ?P4, 'receive', msg_to_p4},
                {trace, ?P3, exit, normal},
                {trace, ?P4, exit, normal}
              ], T3_1 -- T3),

              % All monitors should be terminated.
              ?assertNot(is_process_alive(M1)),
              ?assertNot(is_process_alive(M3)),
              ?assertNot(is_process_alive(M4))
            end
          )}
        end},
      {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
        {delay, 0, {exit, ?P1, normal}},
        {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
        {delay, 0, {fork, ?P3, ?P4, {m4, f4, []}}}],
        fun({m3, f3, []}) ->
          {ok, fun(_) -> 'end' end};
          ({m4, f4, []}) ->
            {ok, fun(_) -> 'end' end};
          ({_, _, _}) ->
            undefined
        end
      },
        fun(_, _) ->
          {"Monitor M1 is shared by P1 and P2; P3 and P4 each have their own
            monitors", ?_test(
            begin

            % Wait until monitoring simulation completes and traces are
            % collected.
              ?small_wait,

              % Monitor M1 should be shared by P1 and P2. It should also be
              % alive even though P1 terminates.
              {M1, G1, T1} = tracer:get_mon_info_rev(?P1),
              {M2, G2, T2} = tracer:get_mon_info_rev(?P2),
              ?assertEqual(M1, M2),
              ?assertEqual(G1, G2),
              ?assertEqual(T1, T2),
              ?assertEqual([?P2], G1),
              ?assert(is_process_alive(M1)),

              % Monitor M3 should be only attached to P3. It should also be
              % alive.
              {M3, G3, T3} = tracer:get_mon_info_rev(?P3),
              ?assertEqual([?P3], G3),
              ?assert(is_process_alive(M3)),

              % Monitor M4 should be only attached to P4. It should also be
              % alive.
              {M4, G4, T4} = tracer:get_mon_info_rev(?P4),
              ?assertEqual([?P4], G4),
              ?assert(is_process_alive(M4)),

              % Monitor M1 receives trace events for P1 and P2.
              ?assertEqual([
                {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                {trace, ?P1, exit, normal},
                {trace, ?P2, spawn, ?P3, {m3, f3, []}}
              ], T1),

              % Monitor M3 receives trace events for P3 only.
              ?assertEqual([
                {trace, ?P3, spawn, ?P4, {m4, f4, []}}
              ], T3),

              % Monitor M4 has no events.
              ?assertEqual([], T4),

              % Post receive events and exit events to terminate P2, P3, P4.
              log_tracer:post_events([
                {delay, 0, {recv, ?P2, msg_to_p2}},
                {delay, 0, {recv, ?P3, msg_to_p3}},
                {delay, 0, {recv, ?P4, msg_to_p4}},
                {delay, 0, {exit, ?P2, normal}},
                {delay, 0, {exit, ?P3, normal}},
                {delay, 0, {exit, ?P4, normal}}
              ]),
              ?small_wait,

              % Monitor M1 receives trace events for P2 only as P1 is
              % terminated.
              {_, _, T1_1} = tracer:get_mon_info(M1),
              ?assertEqual([
                {trace, ?P2, 'receive', msg_to_p2},
                {trace, ?P2, exit, normal}
              ], T1_1 -- T1),

              % Monitor M3 receives trace events for P3 only.
              {_, _, T3_1} = tracer:get_mon_info(M3),
              ?assertEqual([
                {trace, ?P3, 'receive', msg_to_p3},
                {trace, ?P3, exit, normal}
              ], T3_1 -- T3),

              % Monitor M4 receives trace events for P4 only.
              {_, _, T4_1} = tracer:get_mon_info(M4),
              ?assertEqual([
                {trace, ?P4, 'receive', msg_to_p4},
                {trace, ?P4, exit, normal}
              ], T4_1 -- T4),

              % All monitors should be terminated.
              ?assertNot(is_process_alive(M1)),
              ?assertNot(is_process_alive(M3)),
              ?assertNot(is_process_alive(M4))
            end
          )}
        end},
      {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
        {delay, 0, {exit, ?P1, normal}},
        {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
        {delay, 300, {fork, ?P3, ?P4, {m4, f4, []}}}],
        fun({m3, f3, []}) ->
          {ok, fun(_) -> 'end' end};
          ({m4, f4, []}) ->
            {ok, fun(_) -> 'end' end};
          ({_, _, _}) ->
            undefined
        end
      },
        fun(_, _) ->
          {"Monitor M1 is shared by P1 and P2; P3 and P4 each have their own
            monitors (delayed creation of P4)", ?_test(
            begin

            % Wait until monitoring simulation completes and traces are
            % collected.
              ?small_wait,

              % Monitor M1 should be shared by P1 and P2. It should also be
              % alive even though P1 terminates.
              {M1, G1, T1} = tracer:get_mon_info_rev(?P1),
              {M2, G2, T2} = tracer:get_mon_info_rev(?P2),
              ?assertEqual(M1, M2),
              ?assertEqual(G1, G2),
              ?assertEqual(T1, T2),
              ?assertEqual([?P2], G1),
              ?assert(is_process_alive(M1)),

              % Monitor M3 should be only attached to P3. It should also be
              % alive.
              {M3, G3, T3} = tracer:get_mon_info_rev(?P3),
              ?assertEqual([?P3], G3),
              ?assert(is_process_alive(M3)),

              % Monitor M4 should be only attached to P4. It should also be
              % alive.
              {M4, G4, T4} = tracer:get_mon_info_rev(?P4),
              ?assertEqual([?P4], G4),
              ?assert(is_process_alive(M4)),

              % Monitor M1 receives trace events for P1 and P2.
              ?assertEqual([
                {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                {trace, ?P1, exit, normal},
                {trace, ?P2, spawn, ?P3, {m3, f3, []}}
              ], T1),

              % Monitor M3 receives trace events for P3 only.
              ?assertEqual([
                {trace, ?P3, spawn, ?P4, {m4, f4, []}}
              ], T3),

              % Monitor M4 has no events.
              ?assertEqual([], T4),

              % Post receive events and exit events to terminate P2, P3, P4.
              log_tracer:post_events([
                {delay, 0, {recv, ?P2, msg_to_p2}},
                {delay, 0, {recv, ?P3, msg_to_p3}},
                {delay, 0, {recv, ?P4, msg_to_p4}},
                {delay, 0, {exit, ?P2, normal}},
                {delay, 0, {exit, ?P3, normal}},
                {delay, 0, {exit, ?P4, normal}}
              ]),
              ?small_wait,

              % Monitor M1 receives trace events for P2 only as P1 is
              % terminated.
              {_, _, T1_1} = tracer:get_mon_info(M1),
              ?assertEqual([
                {trace, ?P2, 'receive', msg_to_p2},
                {trace, ?P2, exit, normal}
              ], T1_1 -- T1),

              % Monitor M3 receives trace events for P3 only.
              {_, _, T3_1} = tracer:get_mon_info(M3),
              ?assertEqual([
                {trace, ?P3, 'receive', msg_to_p3},
                {trace, ?P3, exit, normal}
              ], T3_1 -- T3),

              % Monitor M4 receives trace events for P4 only.
              {_, _, T4_1} = tracer:get_mon_info(M4),
              ?assertEqual([
                {trace, ?P4, 'receive', msg_to_p4},
                {trace, ?P4, exit, normal}
              ], T4_1 -- T4),

              % All monitors should be terminated.
              ?assertNot(is_process_alive(M1)),
              ?assertNot(is_process_alive(M3)),
              ?assertNot(is_process_alive(M4))
            end
          )}
        end}


%%      {{[
%%
%%      ],
%%        fun({_, _, _}) ->
%%          false
%%        end
%%      },
%%        fun(_, _) ->
%%          {"Tests that the Monitor M1 is shared by all processes", ?_test(
%%            begin
%%
%%            % Wait until monitoring simulation completes and traces are
%%            % collected.
%%              ?small_wait
%%
%%            end
%%          )}
%%        end}

    ]}
}.

%% @doc Tests the dynamic routing logic when a sub-process terminates.
p3_dies_monitor_test_x() -> {"P3 dies when it has a monitor attached test",
  {foreachx,
    fun({Trace, Monitors}) ->

      % Start the monitoring system in offline mode. The system under scrutiny
      % is not run; rather a run simulation is performed by posting trace events
      % to the underlying log tracer.
      monitor:start_offline("", ?P1, Monitors, []),
      log_tracer:post_events(Trace)
    end,
    fun(_, _) ->

      % Stop monitoring system and underlying tracer.
      monitor:stop()
    end,
    [
      {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
        {delay, 0, {exit, ?P1, normal}},
        {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
        {delay, 0, {fork, ?P3, ?P4, {m4, f4, []}}},
        {delay, 0, {exit, ?P3, normal}}],
        fun({m2, f2, []}) ->
          {ok, fun(_) -> 'end' end};
          ({m3, f3, []}) ->
            {ok, fun(_) -> 'end' end};
          ({m4, f4, []}) ->
            {ok, fun(_) -> 'end' end};
          ({_, _, _}) ->
            undefined
        end
      },
        fun(_, _) ->
          {"All processes have dedicated monitors", ?_test(
            begin

            % Wait until monitoring simulation completes and traces are
            % collected.
              ?small_wait,
              ?small_wait,
              ?small_wait,

              % Monitor M1 should be only attached to P1. It should also be
              % terminated since P1 terminates.
              {M1, _G1, T1} = tracer:get_mon_info_rev(?P1),
              ?assertNot(is_process_alive(M1)),

              % Monitor M2 should be only attached to P2. It should also be
              % alive.
              {M2, G2, T2} = tracer:get_mon_info_rev(?P2),
              ?assertEqual([?P2], G2),
              ?assert(is_process_alive(M2)),

              % Monitor M3 should be only attached to P3. It should also be
              % terminated since P3 terminates.
              {M3, _G3, T3} = tracer:get_mon_info_rev(?P3),
              ?assertNot(is_process_alive(M3)),

              % Monitor M4 should be only attached to P4. It should also be
              % alive.
              {M4, G4, T4} = tracer:get_mon_info_rev(?P4),
              ?assertEqual([?P4], G4),
              ?assert(is_process_alive(M4)),

              % Monitor M1 receives trace events for P1 only.
              ?assertEqual([
                {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                {trace, ?P1, exit, normal}
              ], T1),

              % Monitor M2 receives trace events for P2 only.
              ?assertEqual([
                {trace, ?P2, spawn, ?P3, {m3, f3, []}}
              ], T2),

              % Monitor M3 receives trace events for P3 only.
              ?assertEqual([
                {trace, ?P3, spawn, ?P4, {m4, f4, []}},
                {trace, ?P3, exit, normal}
              ], T3),

              % Monitor M4 has no events.
              ?assertEqual([], T4),

              % Post receive events and exit events to terminate P2, P4.
              log_tracer:post_events([
                {delay, 0, {recv, ?P2, msg_to_p2}},
                {delay, 0, {recv, ?P4, msg_to_p4}},
                {delay, 0, {exit, ?P2, normal}},
                {delay, 0, {exit, ?P4, normal}}
              ]),
              ?small_wait,

              % Monitor M2 receives trace events for P2 only.
              {_, _, T2_1} = tracer:get_mon_info(M2),
              ?assertEqual([
                {trace, ?P2, 'receive', msg_to_p2},
                {trace, ?P2, exit, normal}
              ], T2_1 -- T2),

              % Monitor M4 receives trace events for P4 only.
              {_, _, T4_1} = tracer:get_mon_info(M4),
              ?assertEqual([
                {trace, ?P4, 'receive', msg_to_p4},
                {trace, ?P4, exit, normal}
              ], T4_1 -- T4),

              % All monitors should be terminated.
              ?assertNot(is_process_alive(M2)),
              ?assertNot(is_process_alive(M4))
            end
          )}
        end},
      {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}}, % TODO: This is the test that fisted me deeply and thoroughly!
        {delay, 0, {exit, ?P1, normal}},
        {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
        {delay, 0, {fork, ?P3, ?P4, {m4, f4, []}}},
        {delay, 0, {exit, ?P3, normal}}],
        fun({m3, f3, []}) ->
          {ok, fun(_) -> 'end' end};
          ({_, _, _}) ->
            undefined
        end
      },
        fun(_, _) ->
          {"Monitor M1 is shared by P1 and P2; M3 by P3 and P4", ?_test(
            begin

            % Wait until monitoring simulation completes and traces are
            % collected.
              ?small_wait,

              % Monitor M1 should be shared by P1 and P2. It should also be
              % alive even though P1 terminates.
              {M1, G1, T1} = tracer:get_mon_info_rev(?P1),
              {M2, G2, T2} = tracer:get_mon_info_rev(?P2),
              ?assertEqual(M1, M2),
              ?assertEqual(G1, G2),
              ?assertEqual(T1, T2),
              ?assertEqual([?P2], G1),
              ?assert(is_process_alive(M1)),

              % Monitor M3 should be shared by P3 and P4. It should also be
              % alive even though P3 terminates.
              {M3, G3, T3} = tracer:get_mon_info_rev(?P3),
              {M4, G4, T4} = tracer:get_mon_info_rev(?P4),
              ?assertEqual(M3, M4),
              ?assertEqual(G3, G4),
              ?assertEqual(T3, T4),
              ?assertEqual([?P4], G3),
              ?assert(is_process_alive(M3)),

              % Monitor M1 receives trace events for P1 and P2.
              ?assertEqual([
                {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                {trace, ?P1, exit, normal},
                {trace, ?P2, spawn, ?P3, {m3, f3, []}}
              ], T1),

              % Monitor M3 receives trace events for P3 and P4.
              ?assertEqual([
                {trace, ?P3, spawn, ?P4, {m4, f4, []}},
                {trace, ?P3, exit, normal}
              ], T3),

              % Post receive events and exit events to terminate P2, P4.
              log_tracer:post_events([
                {delay, 0, {recv, ?P2, msg_to_p2}},
                {delay, 0, {recv, ?P4, msg_to_p4}},
                {delay, 0, {exit, ?P2, normal}},
                {delay, 0, {exit, ?P4, normal}}
              ]),
              ?small_wait,

              % Monitor M1 receives trace events for P2 only as P1 is
              % terminated.
              {_, _, T1_1} = tracer:get_mon_info(M1),
              ?assertEqual([
                {trace, ?P2, 'receive', msg_to_p2},
                {trace, ?P2, exit, normal}
              ], T1_1 -- T1),

              % Monitor M3 receives trace events for P4 only as P3 is
              % terminated.
              {_, _, T3_1} = tracer:get_mon_info(M3),
              ?assertEqual([
                {trace, ?P4, 'receive', msg_to_p4},
                {trace, ?P4, exit, normal}
              ], T3_1 -- T3),

              % All monitors should be terminated.
              ?assertNot(is_process_alive(M1)),
              ?assertNot(is_process_alive(M3))
            end
          )}
        end},
      {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
        {delay, 0, {exit, ?P1, normal}},
        {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
        {delay, 200, {fork, ?P3, ?P4, {m4, f4, []}}},
        {delay, 0, {exit, ?P3, normal}}],
        fun({m3, f3, []}) ->
          {ok, fun(_) -> 'end' end};
          ({_, _, _}) ->
            undefined
        end
      },
        fun(_, _) ->
          {"Monitor M1 is shared by P1 and P2; M3 by P3 and P4 (delayed
            creation of P4)", ?_test(
            begin

            % Wait until monitoring simulation completes and traces are
            % collected.
              ?small_wait,

              % Monitor M1 should be shared by P1 and P2. It should also be
              % alive even though P1 terminates.
              {M1, G1, T1} = tracer:get_mon_info_rev(?P1),
              {M2, G2, T2} = tracer:get_mon_info_rev(?P2),
              ?assertEqual(M1, M2),
              ?assertEqual(G1, G2),
              ?assertEqual(T1, T2),
              ?assertEqual([?P2], G1),
              ?assert(is_process_alive(M1)),

              % Monitor M3 should be shared by P3 and P4. It should also be
              % alive even though P3 terminates.
              {M3, G3, T3} = tracer:get_mon_info_rev(?P3),
              {M4, G4, T4} = tracer:get_mon_info_rev(?P4),
              ?assertEqual(M3, M4),
              ?assertEqual(G3, G4),
              ?assertEqual(T3, T4),
              ?assertEqual([?P4], G3),
              ?assert(is_process_alive(M3)),

              % Monitor M1 receives trace events for P1 and P2.
              ?assertEqual([
                {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                {trace, ?P1, exit, normal},
                {trace, ?P2, spawn, ?P3, {m3, f3, []}}
              ], T1),

              % Monitor M3 receives trace events for P3 and P4.
              ?assertEqual([
                {trace, ?P3, spawn, ?P4, {m4, f4, []}},
                {trace, ?P3, exit, normal}
              ], T3),

              % Post receive events and exit events to terminate P2, P4.
              log_tracer:post_events([
                {delay, 0, {recv, ?P2, msg_to_p2}},
                {delay, 0, {recv, ?P4, msg_to_p4}},
                {delay, 0, {exit, ?P2, normal}},
                {delay, 0, {exit, ?P4, normal}}
              ]),
              ?small_wait,

              % Monitor M1 receives trace events for P2 only as P1 is
              % terminated.
              {_, _, T1_1} = tracer:get_mon_info(M1),
              ?assertEqual([
                {trace, ?P2, 'receive', msg_to_p2},
                {trace, ?P2, exit, normal}
              ], T1_1 -- T1),

              % Monitor M3 receives trace events for P4 only as P3 is
              % terminated.
              {_, _, T3_1} = tracer:get_mon_info(M3),
              ?assertEqual([
                {trace, ?P4, 'receive', msg_to_p4},
                {trace, ?P4, exit, normal}
              ], T3_1 -- T3),

              % All monitors should be terminated.
              ?assertNot(is_process_alive(M1)),
              ?assertNot(is_process_alive(M3))
            end
          )}
        end},
      {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
        {delay, 0, {exit, ?P1, normal}},
        {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
        {delay, 0, {fork, ?P3, ?P4, {m4, f4, []}}},
        {delay, 200, {exit, ?P3, normal}}],
        fun({m3, f3, []}) ->
          {ok, fun(_) -> 'end' end};
          ({_, _, _}) ->
            undefined
        end
      },
        fun(_, _) ->
          {"Monitor M1 is shared by P1 and P2; M3 by P3 and P4 (delayed
            termination of P3)", ?_test(
            begin

            % Wait until monitoring simulation completes and traces are
            % collected.
              ?small_wait,

              % Monitor M1 should be shared by P1 and P2. It should also be
              % alive even though P1 terminates.
              {M1, G1, T1} = tracer:get_mon_info_rev(?P1),
              {M2, G2, T2} = tracer:get_mon_info_rev(?P2),
              ?assertEqual(M1, M2),
              ?assertEqual(G1, G2),
              ?assertEqual(T1, T2),
              ?assertEqual([?P2], G1),
              ?assert(is_process_alive(M1)),

              % Monitor M3 should be shared by P3 and P4. It should also be
              % alive even though P3 terminates.
              {M3, G3, T3} = tracer:get_mon_info_rev(?P3),
              {M4, G4, T4} = tracer:get_mon_info_rev(?P4),
              ?assertEqual(M3, M4),
              ?assertEqual(G3, G4),
              ?assertEqual(T3, T4),
              ?assertEqual([?P4], G3),
              ?assert(is_process_alive(M3)),

              % Monitor M1 receives trace events for P1 and P2.
              ?assertEqual([
                {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                {trace, ?P1, exit, normal},
                {trace, ?P2, spawn, ?P3, {m3, f3, []}}
              ], T1),

              % Monitor M3 receives trace events for P3 and P4.
              ?assertEqual([
                {trace, ?P3, spawn, ?P4, {m4, f4, []}},
                {trace, ?P3, exit, normal}
              ], T3),

              % Post receive events and exit events to terminate P2, P4.
              log_tracer:post_events([
                {delay, 0, {recv, ?P2, msg_to_p2}},
                {delay, 0, {recv, ?P4, msg_to_p4}},
                {delay, 0, {exit, ?P2, normal}},
                {delay, 0, {exit, ?P4, normal}}
              ]),
              ?small_wait,

              % Monitor M1 receives trace events for P2 only as P1 is
              % terminated.
              {_, _, T1_1} = tracer:get_mon_info(M1),
              ?assertEqual([
                {trace, ?P2, 'receive', msg_to_p2},
                {trace, ?P2, exit, normal}
              ], T1_1 -- T2),

              % Monitor M3 receives trace events for P4 only as P3 is
              % terminated.
              {_, _, T3_1} = tracer:get_mon_info(M3),
              ?assertEqual([
                {trace, ?P4, 'receive', msg_to_p4},
                {trace, ?P4, exit, normal}
              ], T3_1 -- T3),

              % All monitors should be terminated.
              ?assertNot(is_process_alive(M1)),
              ?assertNot(is_process_alive(M3))
            end
          )}
        end},
      {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
        {delay, 0, {exit, ?P1, normal}},
        {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
        {delay, 200, {fork, ?P3, ?P4, {m4, f4, []}}},
        {delay, 0, {exit, ?P3, normal}}],
        fun({m3, f3, []}) ->
          {ok, fun(_) -> 'end' end};
          ({m4, f4, []}) ->
            {ok, fun(_) -> 'end' end};
          ({_, _, _}) ->
            undefined
        end
      },
        fun(_, _) ->
          {"Monitor M1 is shared by P1 and P2; P3 and P4 each have their own
            monitors (delayed creation of P4)", ?_test(
            begin

            % Wait until monitoring simulation completes and traces are
            % collected.
              ?small_wait,

              % Monitor M1 should be shared by P1 and P2. It should also be
              % alive even though P1 terminates.
              {M1, G1, T1} = tracer:get_mon_info_rev(?P1),
              {M2, G2, T2} = tracer:get_mon_info_rev(?P2),
              ?assertEqual(M1, M2),
              ?assertEqual(G1, G2),
              ?assertEqual(T1, T2),
              ?assertEqual([?P2], G1),
              ?assert(is_process_alive(M1)),

              % Monitor M3 should be only attached to P3. It should also be
              % terminated since P3 terminates.
              {M3, _G3, T3} = tracer:get_mon_info_rev(?P3),
              ?assertNot(is_process_alive(M3)),

              % Monitor M4 should be only attached to P4. It should also be
              % alive.
              {M4, G4, T4} = tracer:get_mon_info_rev(?P4),
              ?assertEqual([?P4], G4),
              ?assert(is_process_alive(M4)),

              % Monitor M1 receives trace events for P1 and P2.
              ?assertEqual([
                {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                {trace, ?P1, exit, normal},
                {trace, ?P2, spawn, ?P3, {m3, f3, []}}
              ], T1),

              % Monitor M3 receives trace events for P3 only.
              ?assertEqual([
                {trace, ?P3, spawn, ?P4, {m4, f4, []}},
                {trace, ?P3, exit, normal}
              ], T3),

              % Monitor M4 has no events.
              ?assertEqual([], T4),

              % Post receive events and exit events to terminate P2, P4.
              log_tracer:post_events([
                {delay, 0, {recv, ?P2, msg_to_p2}},
                {delay, 0, {recv, ?P4, msg_to_p4}},
                {delay, 0, {exit, ?P2, normal}},
                {delay, 0, {exit, ?P4, normal}}
              ]),
              ?small_wait,

              % Monitor M1 receives trace events for P2 only as P1 is
              % terminated.
              {_, _, T1_1} = tracer:get_mon_info(M1),
              ?assertEqual([
                {trace, ?P2, 'receive', msg_to_p2},
                {trace, ?P2, exit, normal}
              ], T1_1 -- T1),

              % Monitor M4 receives trace events for P4 only.
              {_, _, T4_1} = tracer:get_mon_info(M4),
              ?assertEqual([
                {trace, ?P4, 'receive', msg_to_p4},
                {trace, ?P4, exit, normal}
              ], T4_1 -- T4),

              % All monitors should be terminated.
              ?assertNot(is_process_alive(M1)),
              ?assertNot(is_process_alive(M4))
            end
          )}
        end}
    ]}
}.

%% @doc Tests the dynamic routing logic and the assignment of one tracer to many
%% sub-sub-processes created by a sub-process.
p3_spawns_multiple_child_processes_test_() ->
  {"P3 spawns multiple processes",
    {foreachx,
      fun({Trace, Monitors}) ->

        % Start the monitoring system in offline mode. The system under scrutiny
        % is not run; rather a run simulation is performed by posting trace events
        % to the underlying log tracer.
        monitor:start_offline("", ?P1, Monitors, []),
        log_tracer:post_events(Trace)
      end,
      fun(_, _) ->

        % Stop monitoring system and underlying tracer.
        monitor:stop()
      end,
      [
        {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
          {delay, 0, {exit, ?P1, normal}},
          {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
          {delay, 0, {fork, ?P3, ?P4, {m4, f4, []}}},
          {delay, 0, {fork, ?P3, ?P5, {m5, f5, []}}}],
          fun({m2, f2, []}) ->
            {ok, fun(_) -> 'end' end};
            ({m3, f3, []}) ->
              {ok, fun(_) -> 'end' end};
            ({m4, f4, []}) ->
              {ok, fun(_) -> 'end' end};
            ({m5, f5, []}) ->
              {ok, fun(_) -> 'end' end};
            ({_, _, _}) ->
              undefined
          end
        },
          fun(_, _) ->
            {"All processes have dedicated monitors", ?_test(
              begin

              % Wait until monitoring simulation completes and traces are
              % collected.
                ?small_wait,

                % Monitor M1 should be only attached to P1. It should also be
                % terminated since P1 terminates.
                {M1, _G1, T1} = tracer:get_mon_info_rev(?P1),
                ?assertNot(is_process_alive(M1)),

                % Monitor M2 should be only attached to P2. It should also be
                % alive.
                {M2, G2, T2} = tracer:get_mon_info_rev(?P2),
                ?assertEqual([?P2], G2),
                ?assert(is_process_alive(M2)),

                % Monitor M3 should be only attached to P3. It should also be
                % terminated since P3 terminates.
                {M3, _G3, T3} = tracer:get_mon_info_rev(?P3),
                ?assert(is_process_alive(M3)),

                % Monitor M4 should be only attached to P4. It should also be
                % alive.
                {M4, G4, T4} = tracer:get_mon_info_rev(?P4),
                ?assertEqual([?P4], G4),
                ?assert(is_process_alive(M4)),

                % Monitor M5 should be only attached to P5. It should also be
                % alive.
                {M5, G5, T5} = tracer:get_mon_info_rev(?P5),
                ?assertEqual([?P5], G5),
                ?assert(is_process_alive(M5)),

                % Monitor M1 receives trace events for P1 only.
                ?assertEqual([
                  {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                  {trace, ?P1, exit, normal}
                ], T1),

                % Monitor M2 receives trace events for P2 only.
                ?assertEqual([
                  {trace, ?P2, spawn, ?P3, {m3, f3, []}}
                ], T2),

                % Monitor M3 receives trace events for P3 only.
                ?assertEqual([
                  {trace, ?P3, spawn, ?P4, {m4, f4, []}},
                  {trace, ?P3, spawn, ?P5, {m5, f5, []}}
                ], T3),

                % Monitor M4 has no events.
                ?assertEqual([], T4),

                % Monitor M5 has no events.
                ?assertEqual([], T5),

                % Post receive events and exit events to terminate P2, P3, P4,
                % P5.
                log_tracer:post_events([
                  {delay, 0, {recv, ?P2, msg_to_p2}},
                  {delay, 0, {recv, ?P3, msg_to_p3}},
                  {delay, 0, {recv, ?P4, msg_to_p4}},
                  {delay, 0, {recv, ?P5, msg_to_p5}},
                  {delay, 0, {exit, ?P2, normal}},
                  {delay, 0, {exit, ?P3, normal}},
                  {delay, 0, {exit, ?P4, normal}},
                  {delay, 0, {exit, ?P5, normal}}
                ]),
                ?small_wait,

                % Monitor M2 receives trace events for P2 only.
                {_, _, T2_1} = tracer:get_mon_info(M2),
                ?assertEqual([
                  {trace, ?P2, 'receive', msg_to_p2},
                  {trace, ?P2, exit, normal}
                ], T2_1 -- T2),

                % Monitor M3 receives trace events for P3 only.
                {_, _, T3_1} = tracer:get_mon_info(M3),
                ?assertEqual([
                  {trace, ?P3, 'receive', msg_to_p3},
                  {trace, ?P3, exit, normal}
                ], T3_1 -- T3),

                % Monitor M4 receives trace events for P4 only.
                {_, _, T4_1} = tracer:get_mon_info(M4),
                ?assertEqual([
                  {trace, ?P4, 'receive', msg_to_p4},
                  {trace, ?P4, exit, normal}
                ], T4_1 -- T4),

                % Monitor M5 receives trace events for P5 only.
                {_, _, T5_1} = tracer:get_mon_info(M5),
                ?assertEqual([
                  {trace, ?P5, 'receive', msg_to_p5},
                  {trace, ?P5, exit, normal}
                ], T5_1 -- T5),

                % All monitors should be terminated.
                ?assertNot(is_process_alive(M2)),
                ?assertNot(is_process_alive(M3)),
                ?assertNot(is_process_alive(M4)),
                ?assertNot(is_process_alive(M5))
              end
            )}
          end},
        {{[{delay, 0, {fork, ?P1, ?P2, {m2, f2, []}}},
          {delay, 0, {exit, ?P1, normal}},
          {delay, 0, {fork, ?P2, ?P3, {m3, f3, []}}},
          {delay, 0, {fork, ?P3, ?P4, {m4, f4, []}}},
          {delay, 200, {fork, ?P3, ?P5, {m5, f5, []}}}],
          fun({m2, f2, []}) ->
            {ok, fun(_) -> 'end' end};
            ({m3, f3, []}) ->
              {ok, fun(_) -> 'end' end};
            ({m4, f4, []}) ->
              {ok, fun(_) -> 'end' end};
            ({m5, f5, []}) ->
              {ok, fun(_) -> 'end' end};
            ({_, _, _}) ->
              undefined
          end
        },
          fun(_, _) ->
            {"All processes have dedicated monitors (delayed creation of P5)",
              ?_test(
                begin

                % Wait until monitoring simulation completes and traces are
                % collected.
                  ?small_wait,


                  % Monitor M1 should be only attached to P1. It should also be
                  % terminated since P1 terminates.
                  {M1, _G1, T1} = tracer:get_mon_info_rev(?P1),
                  ?assertNot(is_process_alive(M1)),

                  % Monitor M2 should be only attached to P2. It should also be
                  % alive.
                  {M2, G2, T2} = tracer:get_mon_info_rev(?P2),
                  ?assertEqual([?P2], G2),
                  ?assert(is_process_alive(M2)),

                  % Monitor M3 should be only attached to P3. It should also be
                  % terminated since P3 terminates.
                  {M3, _G3, T3} = tracer:get_mon_info_rev(?P3),
                  ?assert(is_process_alive(M3)),

                  % Monitor M4 should be only attached to P4. It should also be
                  % alive.
                  {M4, G4, T4} = tracer:get_mon_info_rev(?P4),
                  ?assertEqual([?P4], G4),
                  ?assert(is_process_alive(M4)),

                  % Monitor M5 should be only attached to P5. It should also be
                  % alive.
                  {M5, G5, T5} = tracer:get_mon_info_rev(?P5),
                  ?assertEqual([?P5], G5),
                  ?assert(is_process_alive(M5)),

                  % Monitor M1 receives trace events for P1 only.
                  ?assertEqual([
                    {trace, ?P1, spawn, ?P2, {m2, f2, []}},
                    {trace, ?P1, exit, normal}
                  ], T1),

                  % Monitor M2 receives trace events for P2 only.
                  ?assertEqual([
                    {trace, ?P2, spawn, ?P3, {m3, f3, []}}
                  ], T2),

                  % Monitor M3 receives trace events for P3 only.
                  ?assertEqual([
                    {trace, ?P3, spawn, ?P4, {m4, f4, []}},
                    {trace, ?P3, spawn, ?P5, {m5, f5, []}}
                  ], T3),

                  % Monitor M4 has no events.
                  ?assertEqual([], T4),

                  % Monitor M5 has no events.
                  ?assertEqual([], T5),

                  % Post receive events and exit events to terminate P2, P3, P4,
                  % P5.
                  log_tracer:post_events([
                    {delay, 0, {recv, ?P2, msg_to_p2}},
                    {delay, 0, {recv, ?P3, msg_to_p3}},
                    {delay, 0, {recv, ?P4, msg_to_p4}},
                    {delay, 0, {recv, ?P5, msg_to_p5}},
                    {delay, 0, {exit, ?P2, normal}},
                    {delay, 0, {exit, ?P3, normal}},
                    {delay, 0, {exit, ?P4, normal}},
                    {delay, 0, {exit, ?P5, normal}}
                  ]),
                  ?small_wait,

                  % Monitor M2 receives trace events for P2 only.
                  {_, _, T2_1} = tracer:get_mon_info(M2),
                  ?assertEqual([
                    {trace, ?P2, 'receive', msg_to_p2},
                    {trace, ?P2, exit, normal}
                  ], T2_1 -- T2),

                  % Monitor M3 receives trace events for P3 only.
                  {_, _, T3_1} = tracer:get_mon_info(M3),
                  ?assertEqual([
                    {trace, ?P3, 'receive', msg_to_p3},
                    {trace, ?P3, exit, normal}
                  ], T3_1 -- T3),

                  % Monitor M4 receives trace events for P4 only.
                  {_, _, T4_1} = tracer:get_mon_info(M4),
                  ?assertEqual([
                    {trace, ?P4, 'receive', msg_to_p4},
                    {trace, ?P4, exit, normal}
                  ], T4_1 -- T4),

                  % Monitor M5 receives trace events for P5 only.
                  {_, _, T5_1} = tracer:get_mon_info(M5),
                  ?assertEqual([
                    {trace, ?P5, 'receive', msg_to_p5},
                    {trace, ?P5, exit, normal}
                  ], T5_1 -- T5),

                  % All monitors should be terminated.
                  ?assertNot(is_process_alive(M2)),
                  ?assertNot(is_process_alive(M3)),
                  ?assertNot(is_process_alive(M4)),
                  ?assertNot(is_process_alive(M5))
                end
              )}
          end}
      ]}
  }.
