%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Log tracer tests.
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
-module(log_tracer_test).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Mock PIDs of system processes being monitored.
-define(P1, c:pid(0, 11, 0)).
-define(P2, c:pid(0, 13, 0)).
-define(P3, c:pid(0, 17, 0)).
-define(P4, c:pid(0, 19, 0)).

%% Sleeping time that generates pauses in tests. This is used to enable tests to
%% wait until certain trace processing finishes. Timing, though frowned upon in
%% concurrency, is useful here to avoid complicating the tests tracer module
%% with added synchronization directives.
-define(small_wait, timer:sleep(500)).


%%% ----------------------------------------------------------------------------
%%% Tests.
%%% ----------------------------------------------------------------------------

%% @doc Tests the allocation and deallocation of tracers to processes.
tracer_allocation_test_() -> {"Tracer allocation test",
  {foreach,
    fun() ->

      % Start tracer process.
      log_tracer:start("")
    end,
    fun(_) ->

      % Stop tracer process.
      log_tracer:stop()
    end,
    [
      fun(_) ->
        {"Subscribing to a tracee once succeeds", ?_test(
          begin

          % Allocate current process as the tracer of P1.
            Result = log_tracer:trace(?P1),
            ?assert(Result),
            ?assertEqual(self(), log_tracer:get_tracer(?P1))
          end)}
      end,
      fun(_) ->
        {"Subscribing to a tracee multiple times succeeds only once", ?_test(
          begin

          % Allocate current process as the tracer of P1.
            Result = log_tracer:trace(?P1),
            ?assert(Result),
            ?assertEqual(self(), log_tracer:get_tracer(?P1)),

            % Reallocate current process as the tracer of P1.
            Result2 = log_tracer:trace(?P1),
            ?assertNot(Result2)
          end
        )}
      end,
      fun(_) ->
        {"Unsubscribing an unsubscribed tracee succeeds", ?_test(
          begin

          % Deallocate current process as the tracer of P1.
            Result = log_tracer:clear(?P1),
            ?assert(Result)
          end
        )}
      end,
      fun(_) ->
        {"Unsubscribing a subscribed tracee succeeds", ?_test(
          begin

          % Allocate current process as the tracer of P1.
            Result = log_tracer:trace(?P1),
            ?assert(Result),
            ?assertEqual(self(), log_tracer:get_tracer(?P1)),

            % Deallocate current process as the tracer of P1.
            Result2 = log_tracer:clear(?P1),
            ?assert(Result2)
          end
        )}
      end,
      fun(_) ->
        {"Preempting an unsubscribed tracee does not succeed", ?_test(
          begin

          % Preempt tracer of process P1 with this process as the new tracer.
            Result = log_tracer:preempt(?P1),
            ?assertNot(Result)
          end
        )}
      end,
      fun(_) ->
        {"Preempting a subscribed tracee succeeds", ?_test(
          begin

          % Allocate current process as the tracer of P1.
            Result = log_tracer:trace(?P1),
            ?assert(Result),
            ?assertEqual(self(), log_tracer:get_tracer(?P1)),

            % Preempt original allocated tracer of P1 with the new tracer.
            {Pid, Ref} = util:promise(fun() -> log_tracer:preempt(?P1) end),
            ?assert(util:then(Ref)),
            ?assertEqual(Pid, log_tracer:get_tracer(?P1))
          end
        )}
      end
%%      ,
%%      fun(_) ->
%%        {"This is a test", ?_test(
%%          begin
%%            error(not_implemented)
%%          end
%%        )}
%%      end
    ]}
}.

%% @doc Tests tracer event dispatching for general trace events.
event_dispatch_test_() -> {"General trace event dispatch test",
  {foreachx,
    fun(_) ->

      % Start tracer process.
      log_tracer:start("")
    end,
    fun(_, _) ->

      % Stop tracer process.
      log_tracer:stop()
    end,
    [
      {[{delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {recv, ?P2, msg}}],
        fun(Trace, _) ->
          {"All events are queued when allocation table is empty", ?_test(
            begin

            % Post event trace to log tracer.
              log_tracer:post_events(Trace),

              % Assert that the events in the trace and those queued in the
              % event backlog are identical and in the original order.
              ?assertEqual(Trace, log_tracer:get_backlog())
            end
          )}
        end},
      {[{delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {recv, ?P2, msg}}],
        fun(Trace, _) ->
          {"A process event is queued when no tracer is allocated for the
            process", ?_test(
            begin

            % Allocate current process as the tracer of (non-existing process)
            % P3.
              Result = log_tracer:trace(?P3),
              ?assert(Result),
              ?assertEqual(self(), log_tracer:get_tracer(?P3)),

              % Post event trace to log tracer.
              log_tracer:post_events(Trace),

              % Assert that the events in the trace and those queued in the
              % event backlog are identical and in the original order.
              ?assertEqual(Trace, log_tracer:get_backlog())
            end
          )}
        end},
      {[{delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {recv, ?P2, msg}}],
        fun(Trace, _) ->
          {"A process event is dispatched to the tracer allocated for the
            process", ?_test(
            begin

            % Allocate current process as the tracer of P1. The allocation
            % persists since P1 does not terminate.
              Result = log_tracer:trace(?P1),
              ?assert(Result),
              ?assertEqual(self(), log_tracer:get_tracer(?P1)),

              % Post event trace to log tracer.
              log_tracer:post_events(Trace),

              % Assert that the event dispatched to the tracer for P1 is
              % indeed correct. Dispatching in this case is performed
              % incrementally with each posted trace event (P1 is already
              % allocated a tracer prior to posting the trace), and waiting for
              % it complete is not necessary.
              ?assertEqual(event:to_evm_event(unwrap_delay(hd(Trace))), hd(flush())),

              % Assert that the remaining events in the trace and those queued
              % in the event backlog are identical and in the original order.
              ?assertEqual(tl(Trace), log_tracer:get_backlog())
            end
          )}
        end}
    ]}
}.

%% @doc Tests tracer event dispatching for `fork' trace events, and in
%% particular whether tracers are automatically allocated for forked processes.
fork_dispatch_test_() -> {"Fork trace event dispatch test",
  {foreachx,
    fun(_) ->

      % Start tracer process.
      log_tracer:start("")
    end,
    fun(_, _) ->

      % Stop tracer process.
      log_tracer:stop()
    end,
    [
      {[{delay, 0, {recv, ?P2, msg}},
        {delay, 0, {fork, ?P1, ?P2, {m, f, []}}},
        {delay, 0, {send, ?P1, ?P2, msg}}],

        % This test caters for the scenario where the following race condition
        % arises:
        % Race 1: P1 forks P2.
        %         P1 sends msg to P2.
        %         P2 receives msg.
        %
        % Race 2: P2 receives msg.
        %         P1 forks P2.
        %         P1 sends msg to P2.
        %
        % Race 2 is possible, since P1 and P2 are independent entities, and even
        % though P2 is forked by (and therefore, starts existing after) P1, P2
        % manages to record its 'recv' event before P1 records its 'fork' event.
        % The offline tracing algorithm takes care of handling this case
        % correctly by buffering as necessary. Note that this same guarantee is
        % given by the current implementation of the EVM tracing. This race
        % condition was pointed out in the OOPSLA 2021 submission as a possible
        % bug, but the offline case handles it correctly.
        fun(Trace, _) ->
          {"A process fork event is queued when no tracer is allocated for the
            process", ?_test(
            begin

            % Allocate current process as the tracer of (non-existing process)
            % P3.
              Result = log_tracer:trace(?P3),
              ?assert(Result),
              ?assertEqual(self(), log_tracer:get_tracer(?P3)),

              % Post event trace to log tracer.
              log_tracer:post_events(Trace),

              % Assert that the events in the trace and those queued in the
              % event backlog are identical and in the original order.
              ?assertEqual(Trace, log_tracer:get_backlog())
            end
          )}
        end},
      {[{delay, 0, {recv, ?P2, msg}},
        {delay, 0, {fork, ?P1, ?P2, {m, f, []}}},
        {delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {recv, ?P3, msg}}],

        % This test caters for the scenario where the following race condition
        % arises:
        % Race 1: P1 forks P2.
        %         P1 sends msg to P2.
        %         P2 receives msg.
        %         P3 receives msg from some process not recorded in the trace.
        %
        % Race 2: P2 receives msg.
        %         P1 forks P2.
        %         P1 sends msg to P2.
        %         P3 receives msg from some process not recorded in the trace.
        %
        % Race 2 is possible, since P1 and P2 are independent entities, and even
        % though P2 is forked by (and therefore, starts existing after) P1, P2
        % manages to record its 'recv' event before P1 records its 'fork' event.
        % The offline tracing algorithm takes care of handling this case
        % correctly by buffering as necessary. Note that this same guarantee is
        % given by the current implementation of the EVM tracing. This race
        % condition was pointed out in the OOPSLA 2021 submission as a possible
        % bug, but the offline case handles it correctly.
        fun(Trace, _) ->
          {"A process fork event is dispatched to the tracer allocated for the
            parent; the same tracer is automatically allocated to the forked
            process", ?_test(
            begin

            % Allocate current process as the tracer of P1. The allocation
            % persists since P1 does not terminate.
              Result = log_tracer:trace(?P1),
              ?assert(Result),
              ?assertEqual(self(), log_tracer:get_tracer(?P1)),

              % Post event trace to log tracer.
              log_tracer:post_events(Trace),

              % Assert that the tracer allocated to P1 is also automatically
              % allocated to the forked processes P2. This allocation
              % is performed incrementally with each posted trace event (P1, is
              % already allocated a tracer prior to posting the trace), and
              % waiting for it complete is not necessary.
              ?assertEqual(self(), log_tracer:get_tracer(?P2)),

              % Assert that the events dispatched to the tracer allocated to
              % P1 and P2 are indeed correct and in the expected order.
              % Dispatching in this case is performed incrementally with each
              % posted trace event (P1 is already allocated a tracer prior to
              % posting the trace), and waiting for it complete is not
              % necessary.
              ?assertEqual(to_evm_events(unwrap_delays([lists:nth(2, Trace), lists:nth(1, Trace), lists:nth(3, Trace)])), flush()),

              % Assert that the remaining events in the trace and those queued
              % in the event backlog are identical and in the original order.
              ?assertEqual(lists:nthtail(3, Trace), log_tracer:get_backlog())
            end
          )}
        end},
      {[{delay, 0, {fork, ?P1, ?P2, {m, f, []}}},
        {delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {fork, ?P2, ?P3, {m, f, []}}},
        {delay, 0, {recv, ?P2, msg}},
        {delay, 0, {recv, ?P3, msg}},
        {delay, 0, {send, ?P2, ?P3, msg}}],
        fun(Trace, _) ->
          {"Transitively related process fork events are dispatched to the
            tracer allocated to the root process due to the automatic allocation
            of the tracer to forked processes", ?_test(
            begin

            % Allocate current process as the tracer of P1. The allocation
            % persists since P1 does not terminate.
              Result = log_tracer:trace(?P1),
              ?assert(Result),
              ?assertEqual(self(), log_tracer:get_tracer(?P1)),

              % Post event trace to log tracer.
              log_tracer:post_events(Trace),

              % Assert that the tracer allocated to P1 is also automatically
              % allocated to the forked processes P2 and P3. This allocation
              % is performed incrementally with each posted trace event (P1, is
              % already allocated a tracer prior to posting the trace), and
              % waiting for it complete is not necessary.
              ?assertEqual(self(), log_tracer:get_tracer(?P2)),
              ?assertEqual(self(), log_tracer:get_tracer(?P3)),

              % Assert that the events dispatched to the tracer allocated to
              % P1, P2 and P3 are indeed correct and in the expected order.
              % Dispatching in this case is performed incrementally with each
              % posted trace event (P1 is already allocated a tracer prior to
              % posting the trace), and waiting for it complete is not
              % necessary.
              ?assertEqual(to_evm_events(unwrap_delays(Trace)), flush()),

              % Assert that all events in the backlog have been dispatched.
              ?assertEqual([], log_tracer:get_backlog())
            end
          )}
        end}
    ]}
}.

%% @doc Tests tracer event dispatching for `exit' trace events, and in
%% particular whether tracers are automatically deallocated for terminated
%% processes.
exit_dispatch_test_() -> {"Exit trace event dispatch test",
  {foreachx,
    fun(_) ->

      % Start tracer process.
      log_tracer:start("")
    end,
    fun(_, _) ->

      % Stop tracer process.
      log_tracer:stop()
    end,
    [
      {[{delay, 0, {exit, ?P1, reason}},
        {delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {recv, ?P2, msg}}],
        fun(Trace, _) ->
          {"A process exit event is queued when no tracer is allocated for the
            process", ?_test(
            begin

            % Allocate current process as the tracer of (non-existing process)
            % P3.
              Result = log_tracer:trace(?P3),
              ?assert(Result),
              ?assertEqual(self(), log_tracer:get_tracer(?P3)),

              % Post event trace to log tracer.
              log_tracer:post_events(Trace),

              % Assert that the events in the trace and those queued in the
              % event backlog are identical and in the original order.
              ?assertEqual(Trace, log_tracer:get_backlog())
            end
          )}
        end},
      {[{delay, 0, {exit, ?P1, reason}},
        {delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {recv, ?P2, msg}}],
        fun(Trace, _) ->
          {"A process event is dispatched to the tracer allocated for the
            process; the same tracer is automatically deallocated for the
            terminated process", ?_test(
            begin

            % Allocate current process as the tracer of P1. The allocation
            % does not persist since P1 terminates.
              Result = log_tracer:trace(?P1),
              ?assert(Result),
              ?assertEqual(self(), log_tracer:get_tracer(?P1)),

              % Post event trace to log tracer.
              log_tracer:post_events(Trace),

              % Assert that the tracer allocated to P1 is now deallocated
              % since it terminates. This deallocation is performed
              % incrementally with each posted trace event (P1 is already
              % allocated a tracer prior to posting the trace), and waiting for
              % it to complete is not necessary.
              ?assertEqual(undefined, log_tracer:get_tracer(?P1)),

              % Assert that the event dispatched to the tracer for P1 is
              % indeed correct. Dispatching in this case is performed
              % incrementally with each posted trace event (P1 is already
              % allocated a tracer prior to posting the trace), and waiting for
              % it complete is not necessary.
              ?assertEqual(event:to_evm_event(unwrap_delay(hd(Trace))), hd(flush())),

              % Assert that the remaining events in the trace and those queued
              % in the event backlog are identical and in the original order.
              ?assertEqual(lists:nthtail(1, Trace), log_tracer:get_backlog())
            end
          )}
        end}
    ]}
}.

%% @doc Tests whether the `fork' and `exit' trace events interoperate correctly,
%% and in particular, whether the processing of exit events results in the the
%% deallocation of tracers previously allocated automatically during the
%% processing of `fork' events.
fork_and_exit_dispatch_test_() -> {"Fork and exit interoperation dispatch test",
  {foreachx,
    fun(_) ->

      % Start tracer process.
      log_tracer:start("")
    end,
    fun(_, _) ->

      % Stop tracer process.
      log_tracer:stop()
    end,
    [
      {[{delay, 0, {fork, ?P1, ?P2, {m, f, []}}},
        {delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {exit, ?P1, reason}},
        {delay, 0, {fork, ?P2, ?P3, {m, f, []}}},
        {delay, 0, {recv, ?P2, msg}},
        {delay, 0, {recv, ?P3, msg}},
        {delay, 0, {send, ?P2, ?P3, msg}},
        {delay, 0, {exit, ?P3, reason}},
        {delay, 0, {exit, ?P2, reason}}],
        fun(Trace, _) ->
          {"Transitively related process fork events are dispatched to the
            tracer allocated to the root process due to the automatic allocation
            of the tracer to forked processes; exit events for each process are
            also dispatched to the automatically allocated tracer", ?_test(
            begin

            % Allocate current process as the tracer of P1. The allocation
            % does not persist since P1 terminates.
              Result = log_tracer:trace(?P1),
              ?assert(Result),
              ?assertEqual(self(), log_tracer:get_tracer(?P1)),

              % Post event trace to log tracer.
              log_tracer:post_events(Trace),

              % Assert that the tracer allocated to P1, also automatically
              % allocated to P2 and P3, is now deallocated since all three
              % processes terminate. This deallocation is performed
              % incrementally with each posted trace event (P1 is already
              % allocated a tracer prior to posting the trace), and waiting for
              % it to complete is not necessary.
              ?assertEqual(undefined, log_tracer:get_tracer(?P1)),
              ?assertEqual(undefined, log_tracer:get_tracer(?P2)),
              ?assertEqual(undefined, log_tracer:get_tracer(?P3)),

              % Assert that the events dispatched to the tracer allocated to
              % P1, P2 and P3 are indeed correct and in the expected order.
              % Dispatching in this case is performed incrementally with each
              % posted trace event (P1 is already allocated a tracer prior to
              % posting the trace), and waiting for it complete is not
              % necessary.
              ?assertEqual(to_evm_events(unwrap_delays(Trace)), flush()),

              % Assert that all events in the backlog have been dispatched.
              ?assertEqual([], log_tracer:get_backlog())
            end
          )}
        end}
    ]}
}.

%% @doc Tests that trace events are correctly dispatched.
%%
%% {@par Event dispatching occurs in two occasions:}
%% {@ol
%%   {@item Dispatching of events in the backlog is indirectly triggered by a
%%          manual ({@ie} via {@link log_tracer:trace/1}) allocation of a new
%%          tracer which could potentially could require that certain events in
%%          backlog are dispatched to the newly-allocated tracer
%%   }
%%   {@item Dispatching of 'live' events ({@ie} not the ones stored in the
%%          backlog awaiting dispatching) takes place dynamically via manual
%%          modification of a previously allocated tracer ({@ie} via
%%          {@link log_tracer:preempt/1}), causing events which where previously
%%          dispatched to one tracer to be dispatched to the new tracer
%%          henceforth, following the invocation of the API call.
%%   }
%% }
dynamic_allocation_test_() -> {"Dynamic allocation and backlog processing test",
  {foreachx,
    fun(_) ->

      % Start tracer process.
      log_tracer:start("")
    end,
    fun(_, _) ->

      % Stop tracer process.
      log_tracer:stop()
    end,
    [
      {[{delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {recv, ?P2, msg}}],
        fun(Trace, _) ->
          {"All events are queued when allocation table is empty; when a new
            tracer is manually allocated, the backlog is reprocessed", ?_test(
            begin

            % Post event trace to log tracer.
              log_tracer:post_events(Trace),

              % Allocate current process as the tracer of P1. The allocation
              % persists since P1 does not terminate.
              Result = log_tracer:trace(?P1),
              ?assert(Result),
              ?assertEqual(self(), log_tracer:get_tracer(?P1)),

              % Assert that the event dispatched to the tracer for P1 is
              % indeed correct. Dispatching depends on the speed with which the
              % trace event backlog is processed; wait until it completes.
              wait_backlog(),
              ?assertEqual(event:to_evm_event(unwrap_delay(hd(Trace))), hd(flush())),

              % Assert that the remaining events in the trace and those queued
              % in the event backlog are identical and in the original order.
              ?assertEqual(tl(Trace), log_tracer:get_backlog())
            end
          )}
        end},
      {{[{delay, 0, {fork, ?P1, ?P2, {m, f, []}}},
        {delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {recv, ?P2, msg}}],
        [{delay, 0, {exit, ?P2, reason}},
          {delay, 0, {exit, ?P1, reason}},
          {delay, 0, {exit, ?P3, reason}}]},
        fun({Trace1, Trace2}, _) ->
          {"Process events being dispatched to one tracer process are dispatched
            to a different process once this preempts the former tracer for
            events for the process", ?_test(
            begin

            % Post first part of event trace to log tracer.
              log_tracer:post_events(Trace1),

              % Allocate current process as the tracer of P1. The allocation
              % does not persist since P1 terminates.
              Result = log_tracer:trace(?P1),
              ?assert(Result),
              ?assertEqual(self(), log_tracer:get_tracer(?P1)),

              % Assert that the events dispatched to the tracer allocated to
              % P1 are indeed correct and in the expected order. Dispatching
              % depends on the speed with which the trace event backlog is
              % processed; wait until it completes.
              wait_backlog(),
              ?assertEqual(to_evm_events(unwrap_delays(Trace1)), flush()),

              % Assert that all events in the backlog have been dispatched.
              ?assertEqual([], log_tracer:get_backlog()),

              % Assert that the tracer automatically allocated to P2 is still
              % allocated.
              ?assertEqual(self(), log_tracer:get_tracer(?P2)),

              % Allocate a new tracer to P1. This executes asynchronously
              % until it terminates with a result.
              {Pid, Ref} = util:promise(
                fun() ->
                  Result2 = log_tracer:preempt(?P1),

                  % Wait for trace event to be delivered to this tracer.
                  Wait = fun() -> receive Event = {trace, _, exit, _} -> Event end end,
                  {Result2, Wait()}
                end
              ),
              ?small_wait, % Gives log_tracer time to process preempt request.
              ?assertEqual(Pid, log_tracer:get_tracer(?P1)),

              % Post second part of event trace to log tracer.
              log_tracer:post_events(Trace2),

              % Retrieve result from P1's (now terminated) tracer. Assert
              % that the event dispatched to it is indeed the correct one.
              {Result3, Event} = util:then(Ref),
              ?assert(Result3),
              ?assertEqual(event:to_evm_event(unwrap_delay(lists:nth(2, Trace2))), Event),

              % Assert that the event dispatched to the tracer for P2 is
              % indeed correct. Dispatching in this case is performed
              % incrementally with each posted trace event (P2 is already
              % allocated a tracer prior to posting the trace), and waiting
              % for it to complete is not necessary.
              ?assertEqual(event:to_evm_event(unwrap_delay(hd(Trace2))), hd(flush())),

              % Assert that the tracer allocated to P1 and that allocated to
              % P2 are now deallocated since both processes terminate. This
              % deallocation is performed incrementally with each posted trace
              % event (P1 and P2 are already allocated a tracer prior to
              % posting the trace), and waiting for it to complete is not
              % necessary.
              ?assertEqual(undefined, log_tracer:get_tracer(?P1)),
              ?assertEqual(undefined, log_tracer:get_tracer(?P2)),

              % Assert that the remaining events in the second trace and those
              % queued in the event backlog are identical and in the original
              % order.
              ?assertEqual(lists:nthtail(2, Trace2), log_tracer:get_backlog())
            end
          )}
        end},
      {{[{delay, 0, {fork, ?P1, ?P2, {m, f, []}}},
        {delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {recv, ?P2, msg}}],
        [{delay, 0, {exit, ?P2, reason}},
          {delay, 0, {exit, ?P1, reason}},
          {delay, 0, {exit, ?P3, reason}}]},
        fun({Trace1, Trace2}, _) ->
          {"Process events being dispatched to a tracer process cease to be
            dispatched once the latter is unsubscribed", ?_test(
            begin

            % Post first part of event trace to log tracer.
              log_tracer:post_events(Trace1),

              % Allocate current process as the tracer of P1. The allocation
              % does not persist since P1 terminates.
              Result = log_tracer:trace(?P1),
              ?assert(Result),
              ?assertEqual(self(), log_tracer:get_tracer(?P1)),

              % Assert that the events dispatched to the tracer allocated to
              % P1 are indeed correct and in the expected order. Dispatching
              % depends on the speed with which the trace event backlog is
              % processed; wait until it completes.
              wait_backlog(),
              ?assertEqual(to_evm_events(unwrap_delays(Trace1)), flush()),

              % Assert that all events in the backlog have been dispatched.
              ?assertEqual([], log_tracer:get_backlog()),

              % Deallocate current process as the tracer of P1.
              Result2 = log_tracer:clear(?P1),
              ?assert(Result2),
              ?assertEqual(undefined, log_tracer:get_tracer(?P1)),

              % Assert that the tracer automatically allocated to P2 is still
              % allocated.
              ?assertEqual(self(), log_tracer:get_tracer(?P2)),

              % Post second part of event trace to log tracer.
              log_tracer:post_events(Trace2),

              % Assert that the remaining events in the second trace and those
              % queued in the event backlog are identical and in the original
              % order.
              ?assertEqual(tl(Trace2), log_tracer:get_backlog())
            end
          )}
        end},
      {{[{delay, 0, {fork, ?P1, ?P2, {m, f, []}}},
        {delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {recv, ?P2, msg}}],
        [{delay, 0, {exit, ?P2, reason}},
          {delay, 0, {exit, ?P1, reason}},
          {delay, 0, {exit, ?P3, reason}}]},
        fun({Trace1, Trace2}, _) ->
          {"Process events being dispatched to a tracer process cease to be
            dispatched once the latter is unsubscribed; remaining events are
            dispatched again when a different (or same) tracer resumes the
            subscription",
            ?_test(
              begin

              % Post first part of event trace to log tracer.
                log_tracer:post_events(Trace1),

                % Allocate current process as the tracer of P1. The allocation
                % does not persist since P1 terminates.
                Result = log_tracer:trace(?P1),
                ?assert(Result),
                ?assertEqual(self(), log_tracer:get_tracer(?P1)),

                % Assert that the events dispatched to the tracer allocated to
                % P1 are indeed correct and in the expected order. Dispatching
                % depends on the speed with which the trace event backlog is
                % processed; wait until it completes.
                wait_backlog(),
                ?assertEqual(to_evm_events(unwrap_delays(Trace1)), flush()),

                % Assert that all events in the backlog have been dispatched.
                ?assertEqual([], log_tracer:get_backlog()),

                % Deallocate current process as the tracer of P1.
                Result2 = log_tracer:clear(?P1),
                ?assert(Result2),
                ?assertEqual(undefined, log_tracer:get_tracer(?P1)),

                % Assert that the tracer automatically allocated to P2 is still
                % allocated.
                ?assertEqual(self(), log_tracer:get_tracer(?P2)),

                % Allocate a new tracer to P1. This executes asynchronously
                % until it terminates with a result.
                {Pid, Ref} = util:promise(
                  fun() ->
                    Result2 = log_tracer:trace(?P1),

                    % Wait for trace event to be delivered to this tracer.
                    Wait = fun() -> receive Event = {trace, _, exit, _} -> Event end end,
                    {Result2, Wait()}
                  end
                ),
                ?assertEqual(Pid, log_tracer:get_tracer(?P1)),

                % Post second part of event trace to log tracer.
                log_tracer:post_events(Trace2),

                % Retrieve result from P1's (now terminated) tracer. Assert
                % that the event dispatched to it is indeed the correct one.
                {Result3, Event} = util:then(Ref),
                ?assert(Result3),
                ?assertEqual(event:to_evm_event(unwrap_delay(lists:nth(2, Trace2))), Event),

                % Assert that the event dispatched to the tracer for P2 is
                % indeed correct. Dispatching in this case is performed
                % incrementally with each posted trace event (P2 is already
                % allocated a tracer prior to posting the trace), and waiting
                % for it to complete is not necessary.
                ?assertEqual(event:to_evm_event(unwrap_delay(hd(Trace2))), hd(flush())),

                % Assert that the tracer allocated to P1 and that allocated to
                % P2 are now deallocated since both processes terminate. This
                % deallocation is performed incrementally with each posted trace
                % event (P1 and P2 are already allocated a tracer prior to
                % posting the trace), and waiting for it to complete is not
                % necessary.
                ?assertEqual(undefined, log_tracer:get_tracer(?P1)),
                ?assertEqual(undefined, log_tracer:get_tracer(?P2)),

                % Assert that the remaining events in the second trace and those
                % queued in the event backlog are identical and in the original
                % order.
                ?assertEqual(lists:nthtail(2, Trace2), log_tracer:get_backlog())
              end
            )}
        end}
    ]}
}.


%% @doc Tests that when events appear out of order in the trace, the oldest
%% are dispatched first when a PID relating to these events is allocated a
%% tracer.
%%
%% {@par The automatic allocation of tracers due to forked processes is
%%       respected, and the event backlog is reprocessed each time an automatic
%%       allocation is performed by {@link log_tracer}. This ensures that the
%%       oldest events in the that are kept in the backlog are processed first.
%%       This behavior is required, otherwise events to the tracer may be
%%       dispatched in an order that is not intended, leading to an erroneous
%%       representation of events in the trace.
%% }
%% {@par It is instructive to see why this is required through the following
%%       example.
%% }
%% {@par Suppose that the (out of order) list of trace events
%%   `fork(P2, P3),recv(P2),fork(P1, P2),send(P3, P2),exit(P2)'
%%   currently resides in the trace event backlog maintained by
%%   {@link log_tracer}. An allocation of a new singleton tracer `T_1' for
%%   process `P1' triggers a reprocessing of the backlog as follows:
%% }
%% {@ol
%%   {@item `fork(P2, P3)' is processed and kept in the backlog.}
%%   {@item `recv(P2)' is processed and kept in the backlog.}
%%   {@item `fork(P1, P2)' is processed and dispatched to `T_1'; also, tracer
%%          `T_1' is automatically allocated to the newly-forked process `P1'
%%   }
%% }
%% {@par At this point, the backlog can be processed in one of two ways. If the
%%       remaining events naively employ the new allocation of `T_1' to `P2',
%%       then `send(P3, P2)' is processed and kept in the backlog, followed by
%%       `exit(P2)' which is instantly dispatched to `T_1'. It is not hard to
%%       see that once the trace events at the head of the backlog, namely,
%%       `fork(P2, P3)' and `recv(P2)', are processed, `T_1' is left with the
%%       trace `exit(P2),fork(P2, P3),recv(P2)', which is clearly not
%%       desired as the reordering of events portray an unsound execution for
%%       process `P2'. The problem can be eliminated if the backlog is
%%       reprocessed as <i>soon as a new tracer allocation is detected</i>,
%%       resulting in the trace `fork(P2, P3),recv(P2),exit(P2)' being
%%       instead dispatched to `T_1'.
%% }
event_reordering_test_() -> {"Trace event reordering test",
  {foreachx,
    fun(_) ->

      % Start tracer process.
      log_tracer:start("")
    end,
    fun(_, _) ->

      % Stop tracer process.
      log_tracer:stop()
    end,
    [
      {[{delay, 0, {fork, ?P2, ?P3, {m, f, []}}},
        {delay, 0, {recv, ?P2, msg}},
        {delay, 0, {recv, ?P3, msg}},
        {delay, 0, {fork, ?P1, ?P2, {m, f, []}}},
        {delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {send, ?P2, ?P3, msg}}],
        fun(Trace, _) ->
          {"Process events are dispatched to the tracer in their original order
            (as exhibited in the trace) preserved when a new subscription is
            made; first subscribe tracer to root process, then post trace",
            ?_test(
              begin

              % Allocate current process as the tracer of P1. The allocation
              % persists since P1 does not terminate.
                Result = log_tracer:trace(?P1),
                ?assert(Result),
                ?assertEqual(self(), log_tracer:get_tracer(?P1)),

                % Post event trace to log tracer.
                log_tracer:post_events(Trace),

                % Assert that the tracer allocated to P1 is also automatically
                % allocated to the forked processes P2 and P3. This allocation
                % is performed incrementally with each posted trace event (P1
                % is already allocated a tracer prior to posting the trace),
                % and waiting for it complete is not necessary.
                ?assertEqual(self(), log_tracer:get_tracer(?P2)),
                ?assertEqual(self(), log_tracer:get_tracer(?P3)),

                % Assert that the events dispatched to the tracer are indeed
                % correct and in the expected order. When events are reordered
                % in the tracer, the order in which these are delivered to the
                % tracer depends on its allocation. In this case, P1 is
                % allocated a tracer that is also automatically allocated to P2
                % and P3. Consequently, all events in the trace are dispatched
                % to it.
                Expected = [{fork, ?P1, ?P2, {m, f, []}},
                  {fork, ?P2, ?P3, {m, f, []}},
                  {recv, ?P2, msg},
                  {recv, ?P3, msg},
                  {send, ?P1, ?P2, msg},
                  {send, ?P2, ?P3, msg}],
                ?assertEqual(to_evm_events(Expected), flush()),

                % Assert that all events in the backlog have been dispatched.
                ?assertEqual([], log_tracer:get_backlog())
              end
            )}
        end},
      {[{delay, 0, {fork, ?P2, ?P3, {m, f, []}}},
        {delay, 0, {recv, ?P2, msg}},
        {delay, 0, {recv, ?P3, msg}},
        {delay, 0, {fork, ?P1, ?P2, {m, f, []}}},
        {delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {send, ?P2, ?P3, msg}}],
        fun(Trace, _) ->
          {"Process events are dispatched to the tracer in their original order
            (as exhibited in the trace) preserved when a new subscription is
            made; first post trace, then subscribe tracer to root process",
            ?_test(
              begin

              % Post event trace to log tracer.
                log_tracer:post_events(Trace),

                % Allocate current process as the tracer of P1. The allocation
                % persists since P1 does not terminate.
                Result = log_tracer:trace(?P1),
                ?assert(Result),
                ?assertEqual(self(), log_tracer:get_tracer(?P1)),

                % Assert that the tracer allocated to P1 is also automatically
                % allocated to the forked processes P2 and P3. This allocation
                % depends on the speed with which the trace event backlog is
                % processed; wait until it completes.
                wait_backlog(),
                ?assertEqual(self(), log_tracer:get_tracer(?P2)),
                ?assertEqual(self(), log_tracer:get_tracer(?P3)),

                % Assert that the events dispatched to the tracer are indeed
                % correct and in the expected order. When events are reordered
                % in the tracer, the order in which these are delivered to the
                % tracer depends on its allocation. In this case, P1 is
                % allocated a tracer that is also automatically allocated to P2
                % and P3. Consequently, all events in the trace are dispatched
                % to it. Dispatching depends on the speed with which the trace
                % event backlog is processed; wait until it completes.
                wait_backlog(),
                Expected = [{fork, ?P1, ?P2, {m, f, []}},
                  {fork, ?P2, ?P3, {m, f, []}},
                  {recv, ?P2, msg},
                  {recv, ?P3, msg},
                  {send, ?P1, ?P2, msg},
                  {send, ?P2, ?P3, msg}],
                ?assertEqual(to_evm_events(Expected), flush()),

                % Assert that all events in the backlog have been dispatched.
                ?assertEqual([], log_tracer:get_backlog())
              end
            )}
        end},
      {[{delay, 0, {fork, ?P2, ?P3, {m, f, []}}},
        {delay, 0, {recv, ?P2, msg}},
        {delay, 0, {fork, ?P1, ?P2, {m, f, []}}},
        {delay, 0, {send, ?P3, ?P2, msg}},
        {delay, 0, {exit, ?P3, reason}},
        {delay, 0, {exit, ?P2, reason}},
        {delay, 0, {exit, ?P1, reason}}],
        fun(Trace, _) ->
          {"Process events are dispatched to the tracer in their original order
            (as exhibited in the trace) preserved when a new subscription is
            made; the order of exit events is preserved and kept valid", ?_test(
            begin

            % Post event trace to log tracer.
              log_tracer:post_events(Trace),

              % Allocate current process as the tracer of P1. The allocation
              % does not persist since P1 terminates.
              Result = log_tracer:trace(?P1),
              ?assert(Result),

              % Assert that the tracer allocated to P1, also automatically
              % allocated to P2 and P3, is now deallocated since all three
              % processes terminate. This deallocation depends on the speed with
              % which the trace event backlog is processed; wait until it
              % completes.
              wait_backlog(),
              ?assertEqual(undefined, log_tracer:get_tracer(?P1)),
              ?assertEqual(undefined, log_tracer:get_tracer(?P2)),
              ?assertEqual(undefined, log_tracer:get_tracer(?P3)),

              % Assert that the events dispatched to the tracer are indeed
              % correct and in the expected order. When events are reordered in
              % the trace, the order in which these are delivered to the tracer
              % depends on its allocation. In this case, P1 is allocated a
              % tracer that is also automatically allocated to P2 and P3.
              % Consequently, all events in the trace are dispatched to it.
              Expected = [{fork, ?P1, ?P2, {m, f, []}},
                {fork, ?P2, ?P3, {m, f, []}},
                {recv, ?P2, msg},
                {send, ?P3, ?P2, msg},
                {exit, ?P3, reason},
                {exit, ?P2, reason},
                {exit, ?P1, reason}],
              ?assertEqual(to_evm_events(Expected), flush()),

              % Assert that all events in the backlog have been dispatched.
              ?assertEqual([], log_tracer:get_backlog())
            end
          )}
        end},
      {[{delay, 0, {fork, ?P2, ?P3, {m, f, []}}},
        {delay, 0, {recv, ?P2, msg}},
        {delay, 0, {recv, ?P3, msg}},
        {delay, 0, {fork, ?P1, ?P2, {m, f, []}}},
        {delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {send, ?P2, ?P3, msg}}],
        fun(Trace, _) ->
          {"Process events are dispatched to the tracer in their original order
            (as exhibited in the trace) preserved when a new subscription is
            made; first post trace, then subscribe to tracer to child process.",
            ?_test(
              begin

              % Post event trace to log tracer.
                log_tracer:post_events(Trace),

                % Allocate current process as the tracer of P2. The allocation
                % persists since P2 does not terminate.
                Result = log_tracer:trace(?P2),
                ?assert(Result),
                ?assertEqual(self(), log_tracer:get_tracer(?P2)),

                % Assert that the tracer allocated to P2 is also automatically
                % allocated to the forked process P3. This allocation depends
                % on the speed with which the trace event backlog is processed;
                % wait until it completes.
                wait_backlog(),
                ?assertEqual(self(), log_tracer:get_tracer(?P3)),

                % Assert that the events dispatched to the tracer are indeed
                % correct and in the expected order. When events are reordered
                % in the trace, the order in which there are delivered to the
                % tracer depends on its allocation. In this case, P2 is
                % allocated a tracer, and only its events are dispatched to it.
                % Dispatching depends on the speed with which the trace event
                % backlog is processed; wait until it completes.
                wait_backlog(),
                Expected = [{fork, ?P2, ?P3, {m, f, []}},
                  {recv, ?P2, msg},
                  {recv, ?P3, msg},
                  {send, ?P2, ?P3, msg}],
                ?assertEqual(to_evm_events(Expected), flush()),

                % Assert that the remaining events in the trace and those queued
                % in the event backlog are identical and in the original order.
                Backlog = [{delay, 0, {fork, ?P1, ?P2, {m, f, []}}},
                  {delay, 0, {send, ?P1, ?P2, msg}}],
                ?assertEqual(Backlog, log_tracer:get_backlog())
              end
            )}
        end},
      % T = fork(3,4).send(4,1).exit(4).fork(1,2).recv(2).recv(1).send(1,2).exit(1).fork(2,3).send(2,3).recv(3).exit(3).exit(2)
      % Trace slices are as follows:
      % P1 = fork(1,2).recv(1).send(1,2).exit(1)
      % P2 = recv(2).fork(2,3).send(2,3).exit(2)
      % P3 = fork(3,4).recv(3).exit(3)
      % P4 = send(4,1).exit(4)
      {[{delay, 0, {fork, ?P3, ?P4, {m, f, []}}},
        {delay, 0, {send, ?P4, ?P1, msg}},
        {delay, 0, {exit, ?P4, reason}},
        {delay, 0, {fork, ?P1, ?P2, {m, f, []}}},
        {delay, 0, {recv, ?P2, msg}},
        {delay, 0, {recv, ?P1, msg}},
        {delay, 0, {send, ?P1, ?P2, msg}},
        {delay, 0, {exit, ?P1, reason}},
        {delay, 0, {fork, ?P2, ?P3, {m, f, []}}},
        {delay, 0, {send, ?P2, ?P3, msg}},
        {delay, 0, {recv, ?P3, msg}},
        {delay, 0, {exit, ?P3, reason}},
        {delay, 0, {exit, ?P2, reason}}],
        fun(Trace, _) ->
          {"Process events are dispatched to the tracer in their original order
            (as exhibited in the trace) preserved when a new subscription is
            made; recursive processing of the backlog works correctly", ?_test(
            begin

            % Allocate current process as the tracer of P1. The allocation
            % does not persist since P1 terminates.
              Result = log_tracer:trace(?P1),
              ?assert(Result),

              % Post event trace to log tracer.
              log_tracer:post_events(Trace),

              % Assert that the tracer allocated to P1, also automatically
              % allocated to P2, P3 and P4, is now deallocated since all four
              % processes terminate. This deallocation is performed
              % incrementally with each posted trace event (P1 is already
              % allocated a tracer prior to posting the trace), and waiting for
              % it complete is not necessary.
              ?assertEqual(undefined, log_tracer:get_tracer(?P1)),
              ?assertEqual(undefined, log_tracer:get_tracer(?P2)),
              ?assertEqual(undefined, log_tracer:get_tracer(?P3)),
              ?assertEqual(undefined, log_tracer:get_tracer(?P4)),

              % Assert that the events dispatched to the tracer are indeed
              % correct and in the expected order. When events are reordered in
              % the trace, the order in which these are delivered to the tracer
              % depends on its allocation. In this case, P1 is allocated a
              % tracer that is also automatically allocated to P2, P3 and P4.
              % Consequently, all events in the trace are dispatched to it.
              Expected = [
                {fork, ?P1, ?P2, {m, f, []}},
                {recv, ?P2, msg},
                {recv, ?P1, msg},
                {send, ?P1, ?P2, msg},
                {exit, ?P1, reason},
                {fork, ?P2, ?P3, {m, f, []}},
                {fork, ?P3, ?P4, {m, f, []}},
                {send, ?P4, ?P1, msg},
                {exit, ?P4, reason},
                {send, ?P2, ?P3, msg},
                {recv, ?P3, msg},
                {exit, ?P3, reason},
                {exit, ?P2, reason}
              ],
              ?assertEqual(to_evm_events(Expected), flush()),

              % Assert that all events in the backlog have been dispatched.
              ?assertEqual([], log_tracer:get_backlog())
            end
          )}
        end}
    ]}
}.


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% @doc Returns all the unprocessed messages in the caller's mailbox.
%%
%% {@returns the list of messages in the mailbox.}
-spec flush() -> list().
flush() ->
  receive
    Msg ->
      [Msg | flush()]
  after 200 ->
    []
  end.

%% @doc Returns the event inside the `delay' tuple.
%%
%% {@returns the `event' tuple.}
-spec unwrap_delay(term()) -> term().
unwrap_delay({delay, _, Event}) ->
  Event.

%% @doc Returns the trace stripped of delays.
%%
%% {@returns the list of `event' tuples.}
-spec unwrap_delays(list()) -> list().
unwrap_delays(Trace) when is_list(Trace) ->
  lists:map(fun unwrap_delay/1, Trace).

%% @doc Blocks the caller for a specified amount of time.
%%
%% {@returns `ok'.}
-spec wait_backlog() -> ok.
wait_backlog() ->
  ?small_wait.

%% @private Translates the log events to ones that is identical to what the EVM
%% generates.
to_evm_events([]) ->
  [];
to_evm_events([Event | Events]) ->
  [event:to_evm_event(Event) | to_evm_events(Events)].