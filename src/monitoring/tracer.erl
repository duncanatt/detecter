%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Module description (becomes module heading).
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
-module(tracer).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").
-include("dev.hrl").

%%% Public API.
-export([start/3, stop/0]).

-ifdef(TEST).
-export([get_mon_info/0, get_mon_info/1, set_mon_info/2]).
-export([get_mon_info_rev/0, get_mon_info_rev/1]).
-export([get_proc_mon/1]).
-endif.


%%% Internal callbacks.
-export([tracer/4, tracer/5]).

%%% Types.
-export_type([]).

%%% Implemented behaviors.
%-behavior().


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Two modes a tracer can exist in. In direct mode, the tracer receives events
%% directly from the process(es) it tracers; in priority mode, the tracer
%% may still be receiving events directly from the process(es) it traces, but
%% must nevertheless first analyse those events forwarded to it by other tracers
%% (a.k.a. priority events) before it can then analyse the trace events it
%% receives directly from the processes it traces.
-define(MODE_DIRECT, direct).
-define(MODE_PRIORITY, priority).

%% TODO: What are these? Probably tables used to keep counts used in the ETS table? We'll see
%% TODO: I think they are used in testing to keep the association between tracers and processes.
-ifdef(TEST).
-define(MON_INFO_ETS_NAME, mon_state).
-define(MON_INFO_INV_ETS_NAME, mon_inv_state).
-endif.

%% Maintains the count of the events seen by the tracer.
-record(event_stats, {
  cnt_spawn = 0 :: non_neg_integer(),
  cnt_exit = 0 :: non_neg_integer(),
  cnt_send = 0 :: non_neg_integer(),
  cnt_receive = 0 :: non_neg_integer(),
  cnt_spawned = 0 :: non_neg_integer(),
  cnt_other = 0 :: non_neg_integer()
}).

%% Internal tracer state consisting of:
%%
%% {@dl
%%   {@item `routes'}
%%   {@desc Routing map that determines the next hop from tracer to tracer.}
%%   {@item `group'}
%%   {@desc Processes that are traced by the tracer with their traced mode.}
%%   {@item `mfa_spec'}
%%   {@desc Function that determines whether an analyzer is associated with a
%%          MFA whose process instantiation needs to be monitored.}
%%   {@item `trace'}
%%   {@desc List of trace events seen by the tracer.}
%%   {@item `stats'}
%%   {@desc Counts of the events seen by the tracer.}
%% }
-record(tracer_state, {
  routes = #{} :: routes_table(),
  group = #{} :: group(),
  mfa_spec = fun({_, _, _}) -> undefined end :: analyzer:mfa_spec(),
  trace = [] :: list(),
  stats = #event_stats{} :: event_stats()
}).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type tracer_mode() :: ?MODE_PRIORITY | ?MODE_DIRECT.

-type routes_table() :: #{pid() => pid()}.

-type group() :: #{pid() => tracer_mode()}.

-type parent() :: pid() | self.

-type event_stats() :: #event_stats{}.

-type tracer_state() :: #tracer_state{}.

-type detach() :: {detach, MonPid :: pid(), Pid :: pid()}.

-type routed() :: {route, Router :: pid(), trace_lib:evm_event() | detach()}.

-type message() :: routed() | trace_lib:evm_event().


%%% ----------------------------------------------------------------------------
%%% Data API.
%%% ----------------------------------------------------------------------------

-spec new_mon_stats() -> Stats :: event_stats().
new_mon_stats() ->
  #event_stats{}.

-spec new_mon_stats(CntSpawn, CntExit, CntSend, CntReceive, CntSpawned, CntOther) ->
  Stats :: event_stats()
  when
  CntSpawn :: non_neg_integer(),
  CntExit :: non_neg_integer(),
  CntSend :: non_neg_integer(),
  CntReceive :: non_neg_integer(),
  CntSpawned :: non_neg_integer(),
  CntOther :: non_neg_integer().
new_mon_stats(CntSpawn, CntExit, CntSend, CntReceive, CntSpawned, CntOther) ->
  #event_stats{
    cnt_spawn = CntSpawn, cnt_exit = CntExit, cnt_send = CntSend,
    cnt_receive = CntReceive, cnt_spawned = CntSpawned, cnt_other = CntOther
  }.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

% TODO: We need to add another parameter that specifies whether the analysis
% TODO: is done locally in the tracer or separately in another analyzer process.
-spec start(PidS, MfaSpec, Owner) -> pid()
  when
  PidS :: pid(),
  MfaSpec :: analyzer:mfa_spec(),
  Owner :: parent().
start(PidS, MfaSpec, Owner)
  when is_pid(PidS), is_function(MfaSpec, 1),
  is_pid(Owner); Owner =:= self ->

  ?exec_if_test(io:format("TESTING~n"), io:format("NOT TESTING~n")),
  ?exec_if_test(init_mon_info_tbls(), ok),

  Self = self(),
  spawn(?MODULE, tracer, [PidS, MfaSpec, Self, Owner]).


-spec stop() -> ok.
stop() ->
  ?exec_if_test(
    begin
      ets:delete(?MON_INFO_ETS_NAME),
      ets:delete(?MON_INFO_INV_ETS_NAME)
    end, ok),
  ok.


%%% ----------------------------------------------------------------------------
%%% Internal callbacks.
%%% ----------------------------------------------------------------------------

%% TODO: These functions should be renamed to init.
-spec tracer(PidS, MfaSpec, Parent, Owner) -> no_return()
  when
  PidS :: pid(),
  MfaSpec :: analyzer:mfa_spec(),
  Parent :: pid(),
  Owner :: parent().
tracer(PidS, MfaSpec, Parent, Owner) ->

  ?INFO("Started ROOT tracer ~w for process ~w.", [self(), PidS]),

  % Start tracing root system process. Any trace events will immediately start
  % accumulating in this tracer's mailbox.
  true = trace_lib:trace(PidS),

  % Now that tracing has been started, syn with parent, block and wait until the
  % main tracer loop can be entered.
  util:syn(Parent),

  % Set up new tracer state. Root system PID is added to the new group of
  % processes that are traced by the root tracer.
  State = #tracer_state{
    group = add_proc(PidS, ?MODE_DIRECT, #{}), mfa_spec = MfaSpec
  },
  ?exec_if_test(set_mon_info(PidS, State), ok),

  % Root tracer is started in DIRECT mode since it has no ancestor: this means
  % that it never detaches from an earlier tracer. Being the root, it is also
  % always the case that no trace events can be routed to the root tracer.
  loop(?MODE_DIRECT, State, undefined, Owner).

-spec tracer(PidS, PidT, MonFun, MfaSpec, Owner) -> no_return()
  when
  PidS :: pid(),
  PidT :: pid(),
  MonFun :: analyzer:monitor(),
  MfaSpec :: analyzer:mfa_spec(),
  Owner :: parent().
tracer(PidS, PidT, MonFun, MfaSpec, Owner) ->

  % Spawn monitor process to analyze trace events.
  PidM = analyzer:start(Owner, MonFun),
  ?INFO("Started tracer ~w - monitor ~w for process ~w.", [self(), PidM, PidS]),

  % Detach system process from ancestor tracer and assume control of it in this
  % tracer. The detach command effectively notifies the ancestor tracer that
  % this new tracer has stolen the system process from it.
  detach(PidS, PidT),

  % Set up new tracer state. New system process PID is added to the new group
  % of processes that are traced by this tracer.
  State = #tracer_state{
    group = add_proc(PidS, ?MODE_PRIORITY, #{}), mfa_spec = MfaSpec
  },
  ?exec_if_test(set_mon_info(PidS, State), ok),

  % Tracer is started in PRIORITY mode since it has an ancestor that may
  % potentially route to it trace events that need to be handled before those
  % collected directly via DIRECT tracing.
  loop(?MODE_PRIORITY, State, PidM, Owner).



%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% @private When the tracer is in DIRECT mode, it processes trace events that
%% itself collects directly from the process being analyzed. When in PRIORITY
%% mode, it processes trace events that have been collected by some ancestor
%% tracer, and that have been routed to the current tracer.
-spec loop(Mode, State, PidM, Owner) -> no_return()
  when
  Mode :: tracer_mode(),
  State :: tracer_state(),
  PidM :: pid() | undefined,
  Owner :: parent().
loop(?MODE_PRIORITY, State = #tracer_state{}, PidM, Owner) ->

  % Monitor reference must be a PID when tracer is in PRIORITY mode. It can
  % never have the value undefined (this is only permitted in DIRECT mode).
  ?assert(is_pid(PidM), "Monitor reference is not a PID"),
%%  ?exec_if_test(show_state(State, ?MODE_PRIORITY), ok),

  % When in PRIORITY mode, the tracer dequeues only routed trace events and
  % commands. Not following this protocol could mean the tracer might
  % inadvertently dequeue events that should only be dequeued when in DIRECT
  % mode, reversing the causal order of events. Invalid (junk) events are always
  % dequeued when the tracer is in DIRECT mode (see clause _Other).
  receive
    Msg = {route, PidRtr, Cmd} when element(1, Cmd) =:= detach ->
      ?TRACE("(*) Dequeued relayed command ~w from router tracer ~w.", [Msg, PidRtr]),

      % A routed 'detach' command was dequeued from tracer's mailbox. Command
      % may be relayed to next hop, but if this is not possible (no entry in
      % routes table), it must then be handled by the tracer itself. Even more
      % importantly, is must always be the case that the tracer handling the
      % 'detach' command is the same tracer that issued said command in the
      % first place. For this reason, the tracer PID embedded inside 'detach'
      % must match that of this tracer, i.e., self(). Handling the 'detach'
      % command may cause the tracer to switch to DIRECT mode.
      State0 = handle_detach(State, Msg, PidM, Owner),
      loop(?MODE_PRIORITY, State0, PidM, Owner);

    Msg = {route, PidRtr, Evt} when element(1, Evt) =:= trace ->
      ?TRACE("(*) Dequeued relayed trace event ~w from router tracer ~w.", [Msg, PidRtr]),

      % A routed trace event was dequeued from tracer's mailbox. Event may be
      % relayed to next hop, but if this is not possible (no entry in routes
      % table), it must then be handled by the tracer itself.
      State0 = handle_event(?MODE_PRIORITY, State, Msg, PidM, Owner),
      loop(?MODE_PRIORITY, State0, PidM, Owner)
  end;

loop(?MODE_DIRECT, State = #tracer_state{}, PidM, Owner) ->
%%  ?exec_if_test(show_state(State, ?MODE_DIRECT), ok),

  receive
  %% TODO: This was removed to make it work like the inline version.
%%    Evt when element(1, Evt) =:= trace, element(3, Evt) =:= spawned ->
%%      ?TRACE("(o) Filtered out ~w for process ~w.", [Evt, element(2, Evt)]),
%%
%%      % TODO: We need to handled this event as well to make it equal to the inline.
%%      % Filter out spawned event.
%%      loop(?MODE_DIRECT, State, PidM, Owner);

    Evt when element(1, Evt) =:= trace ->
      ?TRACE("(o) Obtained trace event ~w.", [Evt]),

      % A trace event was dequeued from tracer's mailbox. Event may be relayed
      % to next hop, but if this is not possible (no entry in routes table),
      % it must then be handled by the tracer itself.
      State0 = handle_event(?MODE_DIRECT, State, Evt, PidM, Owner),
      loop(?MODE_DIRECT, State0, PidM, Owner);

    Cmd when element(1, Cmd) =:= detach ->
      ?TRACE("(o) Signalled by command ~w from ~w.", [Cmd, element(2, Cmd)]),

      % A 'detach' command issued by some descendant tracer was dequeued from
      % tracer's mailbox. The 'detach' is always sent to one specific ancestor
      % tracer to notify it that from that point onwards, tracing will be done
      % by the descendant tracer. The fact that this tracer dequeued the
      % 'detach' command implies that this tracer is indeed the ancestor in
      % question: it handles 'detach' by simply routing it to the next hop. This
      % means that it must always be the case that an entry in the routes table
      % exists for said routing to be successful. Absence of such an entry
      % indicates the presence of a bug in the routing algorithm.
      State0 = route_detach(State, Cmd, PidM, Owner),
      loop(?MODE_DIRECT, State0, PidM, Owner);

    Msg = {route, PidRtr, Cmd} when element(1, Cmd) =:= detach ->
      ?TRACE("(o) Dequeued relayed command ~w from router tracer ~w.", [Msg, PidRtr]),

      % A routed 'detach' command was dequeued from tracer's mailbox. A routed
      % 'detach' must always be handled by the tracer when it's in PRIORITY
      % mode. This means that the only possible option for the tracer is to
      % relay the 'detach' command to the next hop. It also implies that it must
      % always be the case that an entry in the routes table exists for said
      % relaying to be successful. Absence of such an entry indicates the
      % presence of a bug in the routing algorithm.
      State0 = relay_detach(State, Msg, PidM, Owner),
      loop(?MODE_DIRECT, State0, PidM, Owner);

    Msg = {route, PidRtr, Evt} when element(1, Evt) =:= trace ->
      ?TRACE("(o) Dequeued relayed trace event ~w from router tracer ~w.", [Msg, PidRtr]),

      % A routed trace event was dequeued from tracer's mailbox. A routed trace
      % event must always be handled by the tracer when it's in PRIORITY mode.
      % This means that the only possible option for the tracer is to relay the
      % event to the next hop. It also implies that it must be the case that an
      % entry in the routes table exists for said relaying to be successful.
      % Absence of such an entry indicates the presence of a bug in the routing
      % algorithm.
      State0 = relay_event(State, Msg),
      loop(?MODE_DIRECT, State0, PidM, Owner);

    _Other ->
      ?ERROR("(o) Dequeued invalid event ~w.", [_Other]),
      error(invalid_event)
  end.


-spec instr(Mode, State, Event, PidT, Owner) -> State0 :: tracer_state()
  when
  Mode :: tracer_mode(),
  State :: tracer_state(),
  Event :: term(),
  PidT :: pid(),
  Owner :: pid().
instr(Mode, State = #tracer_state{},
    {trace, _, spawn, PidTgt, Mfa = {_, _, _}}, PidT, Owner)
  when Mode =:= ?MODE_PRIORITY; Mode =:= ?MODE_DIRECT ->

  % Determine whether a monitor needs to be instrumented for the process spawned
  % as a result of the MFA embedded in the trace event. If no, the process is
  % traced and monitored by the current tracer, otherwise a new tracer and
  % monitor are instrumented.
  case (State#tracer_state.mfa_spec)(Mfa) of
    undefined ->

      ?TRACE("No monitor found for process ~w; adding to own group.", [PidTgt]),

      % When instrumenting a new tracer in PRIORITY mode, it is always the case
      % that the spawned system process in question is currently being traced by
      % some other ancestor tracer (this condition arises from the
      % 'set_on_spawn' semantics). If this was not the case, then the trace
      % event being processes could not have been routed to this tracer. Now, to
      % notify the ancestor monitors that from this point onwards, the current
      % tracer will take over and collect trace events directly for the system
      % process, a detach command is issued to the ancestor. Detach 'steals' the
      % system process from the ancestor tracer, and is required to ensure a
      % correct handover of the tracing responsibility.
      %
      % Detach is not issued in DIRECT mode since the spawn trace event for the
      % system process has been collected directly from the tracer (i.e., was
      % not forwarded and therefore, no ancestor monitor needs to be notified).
      if Mode =:= ?MODE_PRIORITY -> detach(PidTgt, PidT); true -> ok end,

      % New system process is added to the set of processes this tracer
      % currently traces.
      State#tracer_state{
        group = add_proc(PidTgt, Mode, State#tracer_state.group) %% TODO: Test this with hardcoded direct.
%%        group = add_proc(PidTgt, ?MODE_PRIORITY, State#mon_state.group) %% TODO: Test this with hardcoded direct.
      };
    {ok, MonFun} ->

      % Instrument a new tracer for the target process spawned by MFA in the
      % spawn trace event.
      Args = [PidTgt, PidT, MonFun, State#tracer_state.mfa_spec, Owner],
      PidT0 = spawn(?MODULE, tracer, Args),

      ?INFO("Instrumenting monitor ~w on MFA ~w for process ~w.", [PidT0, Mfa, PidTgt]),

      % New system process is NOT added to the set of processes this tracer
      % currently traces: it is being traced by the new tracer. Set up new route
      % in this tracer's routes table to enable it to forward other events for
      % the system process to the new tracer.
      State#tracer_state{
        routes = add_route(PidTgt, PidT0, State#tracer_state.routes)
      }
  end.


%%% ----------------------------------------------------------------------------
%%% Trace event handling functions.
%%% ----------------------------------------------------------------------------

-spec set_stats(Stats, Event) -> Stats0 :: event_stats()
  when
  Stats :: event_stats(),
  Event :: trace_lib:evm_event().
set_stats(Stats = #event_stats{cnt_spawn = Cnt}, {trace, _, spawn, _, _}) ->
  Stats#event_stats{cnt_spawn = Cnt + 1};
set_stats(Stats = #event_stats{cnt_exit = Cnt}, {trace, _, exit, _}) ->
  Stats#event_stats{cnt_exit = Cnt + 1};
set_stats(Stats = #event_stats{cnt_send = Cnt}, {trace, _, send, _, _}) ->
  Stats#event_stats{cnt_send = Cnt + 1};
set_stats(Stats = #event_stats{cnt_receive = Cnt}, {trace, _, 'receive', _}) ->
  Stats#event_stats{cnt_receive = Cnt + 1};
set_stats(Stats = #event_stats{cnt_spawned = Cnt}, {trace, _, spawned, _, _}) ->
  Stats#event_stats{cnt_spawned = Cnt + 1};
set_stats(Stats = #event_stats{cnt_other = Cnt}, Event) when element(1, Event) =:= trace ->
  Stats#event_stats{cnt_other = Cnt + 1}.

-spec set_state(State, Event) -> State0 :: tracer_state()
  when
  State :: tracer_state(),
  Event :: trace_lib:evm_event().
set_state(State = #tracer_state{trace = _Trace, stats = Stats}, Event) ->
  State0 = State#tracer_state{stats = set_stats(Stats, Event)},
  ?exec_if_test(State0#tracer_state{trace = [Event | _Trace]}, State0).

% TODO: Might make sense to split this into functions for DIRECT and PRIORITY.
-spec handle_event(Mode, State, Msg, PidM, Owner) -> State0 :: tracer_state()
  when
  Mode :: tracer_mode(),
  State :: tracer_state(),
  Msg :: message(),
  PidM :: pid(),
  Owner :: parent().
handle_event(?MODE_DIRECT, State, Evt = {trace, PidSrc, spawn, PidTgt, _}, PidM, Owner) ->
  dispatch(PidSrc, State,
    fun _Route(PidT) ->

      % Route trace event to next hop. Routing of a spawn trace event results in
      % the creation of a new system process entry in routes table.
      route(PidT, Evt),
      State#tracer_state{routes = add_route(PidTgt, PidT, State#tracer_state.routes)}
    end,
    fun _Instr() ->

      % Forward trace event to monitor for analysis. Forwarding is done only if
      % monitor is available (not the case for the ROOT tracer, where
      % PidM == undefined).
      if PidM =/= undefined -> analyze(PidM, Evt); true -> ok end,

      % The parameter self() is the PID of the current tracer. When routing, the
      % current tracer becomes the ancestor tracer (or router), i.e., the tracer
      % that initiated the routing procedure for the routed trace event
      % currently being processed. Events are always routed by a tracer when in
      % DIRECT mode, and it is always the case that the routed trace event bears
      % the PID of the router tracer, in the case, self(). Here we are
      % instrumenting a new tracer, passing as an argument self(). This in turn
      % is use by the newly-spawned tracer to issue a detach to the ancestor
      % tracer, which incidentally is this one (i.e., the immediate parent).
      State0 = instr(?MODE_DIRECT, State, Evt, self(), Owner),


%%      ?exec_if_test(
%%        begin
%%          Stats = State0#mon_state.stats#mon_stats{cnt_spawn = State0#mon_state.stats#mon_stats.cnt_spawn + 1},
%%
%%
%%          set_mon_info(PidTgt, State0#mon_state{trace = [Evt | State0#mon_state.trace], stats = Stats})
%%        end, State0)

%%      State1 = save_state(State0, Evt),
%%      ?exec_if_test(set_mon_info(PidTgt, State1), State1)
      State1 = set_state(State0, Evt),
      ?exec_if_test(set_mon_info(PidTgt, State1), State1)

    end);
handle_event(?MODE_DIRECT, State, Evt = {trace, PidSrc, exit, _}, PidM, Owner) ->
  dispatch(PidSrc, State,
    fun _Route(PidT) ->

      % Route trace event to next hop.
      route(PidT, Evt),
      State
    end,
    fun _Clean() ->

      % Forward trace event to monitor for analysis. Forwarding is done only if
      % monitor is available (not the case for the ROOT tracer, where
      % PidM == undefined).
      if PidM =/= undefined -> analyze(PidM, Evt); true -> ok end,

      % System process terminated: remove from tracer's process group.
      State0 = State#tracer_state{group = del_proc(PidSrc, State#tracer_state.group)},

%%      State1 = ?exec_if_test(
%%        begin
%%          Stats = State0#mon_state.stats#mon_stats{cnt_exit = State0#mon_state.stats#mon_stats.cnt_exit + 1},
%%          set_mon_info(PidSrc, State0#mon_state{trace = [Evt | State0#mon_state.trace], stats = Stats})
%%        end, State0),

      State1 = set_state(State0, Evt),
      ?exec_if_test(set_mon_info(PidSrc, State1), State1),


      % Check whether tracer can now be garbage collected.
      try_gc(State1, PidM, Owner)
    end);
handle_event(?MODE_DIRECT, State = #tracer_state{}, Evt, PidM, _) when
  element(3, Evt) =:= send; element(3, Evt) =:= 'receive'; element(3, Evt) =:= spawned ->
  PidSrc = element(2, Evt),
  dispatch(PidSrc, State,
    fun _Route(PidT) ->

      % Route trace event to next hop.
      route(PidT, Evt),
      State
    end,
    fun _Analyze() ->

      % Forward trace event to monitor for analysis. Forwarding is done only if
      % monitor is available (not the case for the ROOT tracer, where
      % PidM == undefined).
      if PidM =/= undefined -> analyze(PidM, Evt); true -> ok end,

%%      ?exec_if_test(
%%        begin
%%          Stats =
%%            if element(3, Evt) =:= send ->
%%              State#mon_state.stats#mon_stats{cnt_send = State#mon_state.stats#mon_stats.cnt_send + 1};
%%              element(3, Evt) =:= 'receive' ->
%%                State#mon_state.stats#mon_stats{cnt_receive = State#mon_state.stats#mon_stats.cnt_receive + 1};
%%              true ->
%%                ?assert(false, format("Unexpected event ~p", [Evt]))
%%            end,
%%          set_mon_info(PidSrc, State#mon_state{trace = [Evt | State#mon_state.trace], stats = Stats})
%%        end, State)

      State0 = set_state(State, Evt),
      ?exec_if_test(set_mon_info(PidSrc, State0), State0)

    end);

% PidRtr is the router, i.e., the ancestor process that kicked off the routing of
% the message.
handle_event(?MODE_PRIORITY, State = #tracer_state{}, Msg = {route, PidRtr, Evt = {trace, PidSrc, spawn, PidTgt, _}}, PidM, Owner) ->
  dispatch(PidSrc, State,
    fun _Relay(PidT) ->

      % Relay routed trace event to next hop. Relaying of a spawn trace event
      % results in the creation of a new system process entry in routes table.
      relay(PidT, Msg),
      State#tracer_state{routes = add_route(PidTgt, PidT, State#tracer_state.routes)}
    end,
    fun _Instr() ->

      % Forward trace event to monitor for analysis.
      analyze(PidM, Msg),

      % The variable PidRtr is the PID of the ancestor tracer (or router), i.e.,
      % the tracer that initiated the routing procedure for the routed trace
      % event currently being processed. Events are always routed by a tracer
      % when in DIRECT mode, and it is always the case that the routed trace
      % event bears the PID of the router tracer, which is self(). Here, we are
      % instrumenting a new tracer, passing as an argument that self() that is
      % stored inside PidRtr. PidRtr is used by the newly-spawned tracer to
      % issue a detach to the ancestor tracer.
      State0 = instr(?MODE_PRIORITY, State, Evt, PidRtr, Owner),
%%      ?exec_if_test(
%%        begin
%%          Stats = State0#mon_state.stats#mon_stats{cnt_spawn = State0#mon_state.stats#mon_stats.cnt_spawn + 1},
%%          set_mon_info(PidTgt, State0#mon_state{trace = [Evt | State0#mon_state.trace], stats = Stats})
%%        end, State0)

      State1 = set_state(State0, Evt),
      ?exec_if_test(set_mon_info(PidTgt, State1), State1)

    end);
handle_event(?MODE_PRIORITY, State = #tracer_state{}, Msg = {route, _PidRtr, Evt = {trace, PidSrc, exit, _}}, PidM, Owner) ->
  dispatch(PidSrc, State,
    fun _Relay(PidT) ->

      % Relay routed trace event to next hop.
      relay(PidT, Msg),
      State
    end,
    fun _Clean() ->

      % Forward trace event to monitor for analysis.
      analyze(PidM, Msg),

      % System process terminated: remove from tracer's process group.
      State0 = State#tracer_state{group = del_proc(PidSrc, State#tracer_state.group)},

%%      State1 = ?exec_if_test(
%%        begin
%%          Stats = State0#mon_state.stats#mon_stats{cnt_exit = State0#mon_state.stats#mon_stats.cnt_exit + 1},
%%          set_mon_info(PidSrc, State0#mon_state{trace = [Evt | State0#mon_state.trace], stats = Stats})
%%        end, State0),

      State1 = set_state(State0, Evt),
      ?exec_if_test(set_mon_info(PidSrc, State1), State1),

      % Check whether tracer can now be garbage collected.
      try_gc(State1, PidM, Owner)
    end);
handle_event(?MODE_PRIORITY, State = #tracer_state{}, Msg = {route, _PidRtr, Evt}, PidM, _) when
  element(3, Evt) =:= send; element(3, Evt) =:= 'receive'; element(3, Evt) =:= spawned ->
  PidSrc = element(2, Evt),
  dispatch(PidSrc, State,
    fun _Relay(PidT) ->

      % Relay routed trace event to next hop.
      relay(PidT, Msg),
      State
    end,
    fun _Analyze() ->

      % Forward trace event to monitor for analysis.
      analyze(PidM, Msg),

%%      ?exec_if_test(
%%        begin
%%          Stats =
%%            if element(3, Evt) =:= send ->
%%              State#mon_state.stats#mon_stats{cnt_send = State#mon_state.stats#mon_stats.cnt_send + 1};
%%              element(3, Evt) =:= 'receive' ->
%%                State#mon_state.stats#mon_stats{cnt_receive = State#mon_state.stats#mon_stats.cnt_receive + 1};
%%              true ->
%%                ?assert(false, format("Unexpected event ~p", [Evt]))
%%            end,
%%          set_mon_info(PidSrc, State#mon_state{trace = [Evt | State#mon_state.trace], stats = Stats})
%%        end, State)

      State0 = set_state(State, Evt),
      ?exec_if_test(set_mon_info(PidSrc, State0), State0)
    end);

handle_event(_, State, Msg, _, _) ->
  ?WARN("Not handling trace event ~p.", [Msg]),
  State.




% The assertions serve to guard against not handling the relevant messages while
% in PRIORITY mode.
-spec route_detach(State, Cmd, PidM, Owner) -> State0 :: tracer_state() | no_return()
  when
  State :: tracer_state(),
  Cmd :: detach(),
  PidM :: pid(),
  Owner :: parent().
route_detach(State, Cmd = {detach, PidT, PidTgt}, PidM, Owner) ->
  dispatch(PidTgt, State,
    fun _Route(PidT) ->

      % Route 'detach' command to next hop. Since this tracer is routing the
      % command, it must be the case that it is the ancestor tracer for whom the
      % command was intended. A 'detach' command is issued by a descendant
      % tracer to inform it's ancestor that it is now capable of tracing its
      % system process DIRECTLY. This implies that the entry for that tracer
      % in this tracer's routes table is now stale, since the latter does not
      % need to route trace events to it any longer. Therefore, the entry in
      % question can be safely removed from the routes table.
      route(PidT, Cmd),
      State0 = State#tracer_state{routes = del_route(PidTgt, State#tracer_state.routes)},

      % Check whether tracer can now be garbage collected.
      try_gc(State0, PidM, Owner)
    end,
    fun _Invalid() ->

      % INVARIANT: It is always the case that a 'detach' command is issued by
      % a descendant tracer to one specific ancestor tracer. This means that
      % sometime in the past, an entry for the descendant tracer must have been
      % created in the routes table of this tracer - it's ancestor. The latter
      % should always relay (albeit indirectly) the 'detach' command back to the
      % originator.
      %
      % Therefore, if this case in the algorithm is reached, the 'detach'
      % command must have been sent to this tracer by mistake, since it should,
      % but cannot be routed back. Fail.
      ?assert(false, format("Detach command from tracer ~w not expected", [PidT]))
    end).

-spec relay_detach(State, Msg, PidM, Owner) -> State0 :: tracer_state() | no_return()
  when
  State :: tracer_state(),
  Msg :: message(),
  PidM :: pid(),
  Owner :: parent().
relay_detach(State, Msg = {route, _, {detach, _, PidTgt}}, PidM, Owner) ->
  dispatch(PidTgt, State,
    fun _Relay(PidT) ->

      % Relay 'detach' command to next hop. Relaying of 'detach' results in the
      % removal of the corresponding system process' entry from tracer's routes
      % table.
      relay(PidT, Msg),
      State0 = State#tracer_state{routes = del_route(PidTgt, State#tracer_state.routes)},
      try_gc(State0, PidM, Owner)
    end,
    fun _DetachOfAlreadyTerminatedProcess() ->


      % TODO: Revisit comments.
      % One expects that a 'detach' command routed to this monitor in DIRECT
      % mode should not be possible due to the fact that routed commands should
      % be handled while the monitor is in PRIORITY mode. But this is not always
      % the case due to a particular race condition that may occur. Before
      % explaining, keep in mind that 'detach' commands are ONLY used to (i)
      % clear the routing table of ancestor monitors of stale routing entries,
      % (ii) contribute to eventually switch the monitor that originally issued
      % the 'detach' command to DIRECT mode. This means that a 'lost' 'detach'
      % command will not be of any consequence.
      %
      % Now, there are cases when a 'detach' command for a particular process is
      % relayed to the monitor that originally issued it long after the process
      % in question has been removed from the process group for the monitor
      % WHEN the monitor is in DIRECT mode. This can occur in scenarios similar
      % to the following.
      %
      % Suppose the system system consists of two processes, P and Q, where Q
      % is spawned by P, and the execution of P and Q finishes before any
      % monitor has been created. For instance, the trace 'spawn(P).spawn(Q).
      % exit(Q)'. There are two cases to consider:
      % 1. When P and Q are monitored by separate monitors MP and MQ.
      %    When the ROOT monitor processes 'spawn(P)' it will create a monitor
      %    MP for P, and issue a 'detach' command to ROOT. MP is now in PRIORITY
      %    mode with P added to Group. Next, 'spawn(Q) is routed to MP, which in
      %    turn, creates a new monitor MQ, and also issues a second 'detach'
      %    command to ROOT. MQ is now in PRIORITY mode with Q added to Group.
      %    Finally, 'exit(Q)' is routed to MQ via MP and MQ removes Q from
      %    Group. Since both Group and Routes in MQ are empty, MQ terminates.
      %    Eventually the 'detach' command for Q is routed to monitor MQ which
      %    is non existent. That is, the 'detach' command for MQ is lost, but
      %    the monitor structure is still sound.
      % 2. When P and Q are monitored by the same monitor MPQ.
      %    When the ROOT monitor processes 'spawn(P) it will create a monitor
      %    MPQ for P, and issue a 'detach' command to ROOT. MPQ is now in
      %    priority mode and P added to Group. Next, 'spawn(Q)' is routed to
      %    MPQ, and Q is also added to Group. Finally, 'exit(Q)' is routed to
      %    MPQ, and MPQ removes Q from Group, leaving only P. Note that Q was
      %    removed while it was still marked as in PRIORITY. Eventually, the
      %    'detach' command for P reaches MPQ: this marks P as DIRECT in Group.
      %    Since in MPQ, Routes is empty and all processes (i.e., just P) in
      %    Group are marked as DIRECT, MPQ switches to DIRECT mode. Right after,
      %    the 'detach' command for Q reaches MPQ as well, but this cannot be
      %    be routed (Routes for MPQ is empty), so it must be handled (this
      %    case). Process Q has been removed from Group, thus the 'detach'
      %    command for MPQ is lost, but as in case 1, the monitor structure is
      %    still sound.



      ?TRACE("Routed 'detach' command handled for (already) terminated process ~w.", [PidTgt]),
      State
    end).


-spec relay_event(State, Msg) -> State0 :: tracer_state()
  when
  State :: tracer_state(),
  Msg :: message().
relay_event(State, Msg = {route, _, {trace, PidSrc, spawn, PidTgt, _}}) ->
  dispatch(PidSrc, State,
    fun _Relay(PidT) ->

      % Relay routed trace event to next hop. Relaying of a spawn event results
      % in the creation of a new system process' entry in tracer's routes table.
      relay(PidT, Msg),
      State#tracer_state{routes = add_route(PidTgt, PidT, State#tracer_state.routes)}
    end,
    fun _Invalid() ->
      ?assert(false, format("Routed trace event ~w cannot be handled while in ~s mode", [Msg, ?MODE_DIRECT]))
    end);
relay_event(State, Msg = {route, _, {trace, PidSrc, exit, _}}) ->
  dispatch(PidSrc, State,
    fun _Relay(PidT) ->

      % Relay routed trace event to next hop.
      relay(PidT, Msg),
      State
    end,
    fun _Invalid() ->
      ?assert(false, format("Routed trace event ~w cannot be handled while in ~s mode", [Msg, ?MODE_DIRECT]))
    end);
relay_event(State = #tracer_state{}, Msg = {route, _, Evt}) when
  element(3, Evt) =:= send; element(3, Evt) =:= 'receive'; element(3, Evt) =:= spawned ->
  PidSrc = element(2, Evt),
  dispatch(PidSrc, State,
    fun _Route(PidT) ->

      % Route trace event to next hop.


%%      route(PidM, Evt), % TODO: Why is this route, and not relay? Probably is a bug, so going to fix it.

      % Relay routed trace event to next hop.
      relay(PidT, Msg),
      State
    end,
    fun _Invalid() ->
      ?assert(false, format("Routed trace event ~w cannot be handled while in ~s mode", [Msg, ?MODE_DIRECT]))
    end).


%% Priority functions (only detach because handle_event is above).
-spec handle_detach(State, Msg, PidM, Owner) -> UpdatedState :: tracer_state() | no_return()
  when
  State :: tracer_state(),
  Msg :: message(),
  PidM :: pid(),
  Owner :: parent().
handle_detach(State, Msg = {route, _, {detach, Self, PidTgt}}, PidM, Owner) ->
  dispatch(PidTgt, State,
    fun _Relay(PidT) ->

      % Relay routed 'detach' command to next hop. Relaying of 'detach' results
      % in the removal of the corresponding system process' entry from tracer's
      % routes table.
      relay(PidT, Msg),
      State0 = State#tracer_state{routes = del_route(PidTgt, State#tracer_state.routes)},
      try_gc(State0, PidM, Owner)
    end,
    fun _Mode() ->

      % INVARIANT: A 'detach' command that is not relayed must be handled by the
      % tracer when in PRIORITY mode. It must always be the case that the tracer
      % handling the 'detach' command is the same tracer that issued said
      % command. For this reason, the tracer PID embedded inside 'detach; must
      % match that of this tracer, i.e., self().
      ?assertEqual(self(), Self, format("Tracer ~w is not equal to ~w for detach command", [Self, self()])),

      % The 'detach' command is interpreted as an 'end of trace' marker by this
      % (i.e., the issuing) tracer. The system process in this tracer's process
      % group must be updated so that its state indicates it is now in DIRECT
      % (or detached) mode. Being in DIRECT mode means that the particular
      % system processes is being traced directly by the tracer through its
      % local trace partition.
      State0 = State#tracer_state{group = sub_proc(PidTgt, ?MODE_DIRECT, State#tracer_state.group)},

      % Check whether the tracer can be switched to DIRECT mode. This is only
      % possible if all of its system processes assigned to it are themselves
      % in DIRECT mode.
      case can_detach(State0) of
        true ->

          % INVARIANT: When switching to direct mode, all system processes
          % assigned to the tracer must likewise be in DIRECT mode.
          ?assertEqual(map_size(State0#tracer_state.group),
            map_size(maps:filter(fun(_, ?MODE_DIRECT) -> true; (_, _) -> false end, State0#tracer_state.group))),

          ?TRACE("Tracer ~w switching to ~w mode.", [self(), ?MODE_DIRECT]),
          loop(?MODE_DIRECT, State0, PidM, Owner);
        false ->
          State0
      end
    end).


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

-spec add_proc(PidS, Mode, Group) -> UpdatedGroup :: group()
  when
  PidS :: pid(),
  Mode :: tracer_mode(),
  Group :: group().
add_proc(PidS, Mode, Group) ->
  ?assertNot(maps:is_key(PidS, Group),
    format("Process ~w must not exist when adding", [PidS])),
  Group#{PidS => Mode}.

-spec del_proc(PidS :: pid(), Group :: group()) -> UpdatedGroup :: group().
del_proc(PidS, Group) ->
  % ?assert(maps:is_key(PidS, Group), % TODO: Commented this for now since might be a potential bug.
  %   format("Process ~w must exist when deleting", [PidS])),
  ?TRACE("Process ~w deleted from group while in ~w mode.", [PidS, maps:get(PidS, Group)]),
  maps:remove(PidS, Group).

-spec sub_proc(PidS, NewMode, Group) -> UpdatedGroup :: group()
  when
  PidS :: pid(),
  NewMode :: tracer_mode(),
  Group :: group().
sub_proc(PidS, NewMode, Group) ->
  % It may be the case that the process we are trying to update does not exist.
  % This happens when a process has exited and is removed from the process group
  % BEFORE the detach command reaches the monitor and there would be no process
  % to update. In this case do nothing and leave group unmodified.
  case maps:is_key(PidS, Group) of
    true ->
      Group#{PidS := NewMode}; % Only update existing value, otherwise fail.
    false ->
      ?TRACE("Process ~w not updated since it is no longer in group.", [PidS]),
      Group
  end.

-spec add_route(PidS :: pid(), PidT :: pid(), Routes :: routes_table()) ->
  UpdatedRoutes :: routes_table().
add_route(PidS, PidT, Routes) ->
  ?assertNot(maps:is_key(PidS, Routes)),
  Routes#{PidS => PidT}.

-spec del_route(PidS :: pid(), Routes :: routes_table()) ->
  UpdatedRoutes :: routes_table().
del_route(PidS, Routes) ->
  ?assert(maps:is_key(PidS, Routes)),
  maps:remove(PidS, Routes).

-spec can_detach(State :: tracer_state()) -> boolean().
can_detach(#tracer_state{group = Group}) ->
  length(lists:filter(
    fun(?MODE_DIRECT) -> false; (_) -> true end, maps:values(Group)
  )) =:= 0.

-spec detach(PidS :: pid(), PidT :: pid()) -> Detach :: detach().
detach(PidS, PidT) ->

  % Preempt former (ancestor) tracer. This tracer now takes over the tracing of
  % the system process: this gives rise to a new 'trace partition'. Ancestor
  % tracer is informed that this tracer (i.e., self()) issued preempt, and that
  % moreover, tracing of the system process will be done by this tracer in turn.
  % Note that there is one specific case where preempt does not succeed (i.e.,
  % returns 'false'): this arises whenever preempt is invoked on a process that
  % has exited before the call to preempt was made. This is perfectly normal,
  % since the tracer is asynchronous, and may process event long after the
  % system process in under scrutiny has terminated.
  trace_lib:preempt(PidS),
  PidT ! {detach, self(), PidS}.

-spec try_gc(State, PidM, Owner) -> State :: tracer_state() | no_return()
  when
  State :: tracer_state(),
  PidM :: pid() | undefined,
  Owner :: parent().
try_gc(#tracer_state{group = Group, routes = Routes, trace = _Trace, stats = Stats}, undefined, Owner) when
  map_size(Group) =:= 0, map_size(Routes) =:= 0 ->

  ?DEBUG("Terminated ROOT tracer ~w.", [self()]),
%%  ?TRACE("Terminated ROOT tracer ~w with trace ~p.", [self(), Trace]),

  % Link to owner process (if Owner =/= self) and exit. Stats are embedded in
  % the exit signal so that these can be collected if Owner is trapping exits.
  if is_pid(Owner) -> link(Owner); true -> ok end,
  exit({garbage_collect, {root, Stats}});

try_gc(#tracer_state{group = Group, routes = Routes, trace = _Trace, stats = Stats}, PidM, Owner) when
  map_size(Group) =:= 0, map_size(Routes) =:= 0 ->

  % Issue stop command to monitor. Monitor will eventually process the command
  % and terminate its execution.
  analyzer:stop(PidM),

  ?DEBUG("Terminated tracer ~w for monitor ~w.", [self(), PidM]),
%%  ?TRACE("Terminated tracer ~w for monitor ~w with trace ~p.",
%%    [self(), PidM, Trace]),

  % Link to owner process (if Owner =/= self) and exit. Stats are embedded in
  % the exit signal so that these can be collected if Owner is trapping exits.
  % Note: Stats are sent from the tracer (rather than the monitor), since the
  % monitor might not process all trace events before reaching a verdict, and
  % the stats collected up to that point would not reflect the true count. While
  % this may still be solved by post processing the monitor's mailbox, it would
  % needlessly complicate its code.
  if is_pid(Owner) -> link(Owner); true -> ok end,
  exit({garbage_collect, {tracer, Stats}});
try_gc(State = #tracer_state{}, _, _) ->
  State.

-spec route(PidT, Msg) -> Routed :: routed()
  when
  PidT :: pid(),
  Msg :: trace_lib:evm_event() | detach().
route(PidT, Msg) when element(1, Msg) =:= trace; element(1, Msg) =:= detach ->
  ?TRACE("Tracer ~w routing ~w to next tracer ~w.", [self(), Msg, PidT]),
  PidT ! {route, self(), Msg}.

-spec relay(PidT :: pid(), Routed :: routed()) -> Routed :: routed().
relay(PidT, Routed) when element(1, Routed) =:= route ->
  ?TRACE("Tracer ~w relaying ~w to next tracer ~w.", [self(), Routed, PidT]),
  PidT ! Routed.

-spec analyze(PidM :: pid(), Msg :: message()) -> Msg :: message().
analyze(PidM, {route, _, Msg}) when element(1, Msg) =:= trace ->
  ?TRACE("Tracer ~w sent routed trace event ~w to monitor ~w for analysis.",
    [self(), Msg, PidM]),
  PidM ! Msg;
analyze(PidM, Msg) when element(1, Msg) =:= trace ->
  ?TRACE("Tracer ~w sent direct trace event ~w to monitor ~w for analysis.",
    [self(), Msg, PidM]),
  PidM ! Msg.

-spec dispatch(PidSrc, State, Forward, Handle) -> Result :: term()
  when
  PidSrc :: pid(),
  State :: tracer_state(),
  Forward :: fun((NextHop :: pid()) -> term()),
  Handle :: fun(() -> term()).
dispatch(PidSrc, #tracer_state{routes = Routes, group = Group}, Forward, Handle)
  when is_function(Handle), is_function(Forward, 1) ->

  % In general, the process PID cannot be traced by this tracer (i.e., be in its
  % process group) and at the same time, be contained in the tracer's routes
  % table: this would seem to suggest that the process is also being traced by
  % another tracer. There is one case however, where this statement is not true.
  % When the process PID is not in the tracer's process group, it could also
  % mean that the process in question exited before its corresponding 'detach'
  % command has been processed (i.e., 'detach' would eventually be processed,
  % but the process referred to by the 'detach' is already removed from the
  % process group). Therefore, the assumption that if the process is not in the
  % group, then it must be in the tracer's routes table is WRONG. Yet, the
  % reverse must always hold: a process that is in the tracer's routes table can
  % never be in its process group as well. This means that R -> not G, or stated
  % differently, not R or not G.
  ?assert((not maps:is_key(PidSrc, Routes)) or not maps:is_key(PidSrc, Group)),

  case maps:get(PidSrc, Routes, undefined) of
    undefined ->
      Handle();
    NextHop ->
      Forward(NextHop)
  end.

%% @private Returns a character list that represents data formatted in
%% accordance with the specified format.
-spec format(Format :: string(), Args :: list()) -> String :: string().
format(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).


%%% ----------------------------------------------------------------------------
%%% Private helper functions defined for testing.
%%% ----------------------------------------------------------------------------


-spec cum_sum_stats(Stats0, Stats1) -> Stats2 :: event_stats()
  when
  Stats0 :: event_stats(),
  Stats1 :: event_stats().
cum_sum_stats(Stats0 = #event_stats{cnt_spawn = Spawn0, cnt_exit = Exit0, cnt_send = Send0, cnt_receive = Receive0, cnt_spawned = Spawned0, cnt_other = Other0},
    #event_stats{cnt_spawn = Spawn1, cnt_exit = Exit1, cnt_send = Send1, cnt_receive = Receive1, cnt_spawned = Spawned1, cnt_other = Other1}) ->
  Stats0#event_stats{cnt_spawn = Spawn0 + Spawn1, cnt_exit = Exit0 + Exit1, cnt_send = Send0 + Send1, cnt_receive = Receive0 + Receive1, cnt_spawned = Spawned0 + Spawned1, cnt_other = Other0 + Other1}.

-spec show_stats(Stats0 :: event_stats(), Stats1 :: event_stats()) -> ok.
show_stats(Stats = #event_stats{}, #event_stats{cnt_send = CntSend, cnt_receive = CntRecv, cnt_other = CntTerm}) ->

  % Calculate the number of expected send and receive trace event messages.
  CntSend0 = CntSend + CntRecv + CntTerm,
  CntRecv0 = CntRecv + CntSend + CntTerm,

  Title = format("Trace Summary", []),
  S0 = color_by_pid(self(), format("~n~64c[ ~s ]~64c~n", [$-, Title, $-])) ++
    format("~-8.s ~b~n", ["Spawn:", Stats#event_stats.cnt_spawn]) ++
    format("~-8.s ~b~n", ["Exit:", Stats#event_stats.cnt_exit]) ++
    format("~-8.s ~b (expected ~b, ~.4f% loss)~n", ["Send:", Stats#event_stats.cnt_send, CntSend0, (CntSend0 - Stats#event_stats.cnt_send) / CntSend0 * 100]) ++
    format("~-8.s ~b (expected ~b, ~.4f% loss)~n", ["Receive:", Stats#event_stats.cnt_receive, CntRecv0, (CntRecv0 - Stats#event_stats.cnt_receive) / CntRecv0 * 100]) ++
    format("~-8.s ~b~n", ["Spawned:", Stats#event_stats.cnt_spawned]) ++
    format("~-8.s ~b~n", ["Other:", Stats#event_stats.cnt_other]) ++
    color_by_pid(self(), format("~" ++ integer_to_list(length(Title) + (64 * 2) + 4) ++ "c~n", [$-])),
  io:put_chars(user, S0).

-spec color_by_pid(Pid :: pid(), Text :: string()) -> iolist().
color_by_pid(Pid, Text) when is_pid(Pid) ->
  {_, N, _} = util:pid_tokens(Pid),
  Code = N rem 255,
  ["\e[38;5;", integer_to_list(Code), "m", Text, "\e[0m"].


-ifdef(TEST).

-spec show_state(State :: tracer_state(), Mode :: tracer_mode()) -> ok.
show_state(State, Mode) ->
  {messages, MQueue} = erlang:process_info(self(), messages),
  Symbol = if Mode =:= ?MODE_DIRECT -> $o; Mode =:= ?MODE_PRIORITY -> $* end,

  Title = format("(~c) Tracer ~w", [Symbol, self()]),
  S0 = color_by_pid(self(), format("~n~64c[ ~s ]~64c~n", [$-, Title, $-])) ++
    format("~-8.s ~p~n", ["Routes:", State#tracer_state.routes]) ++
    format("~-8.s ~p~n", ["Group:", State#tracer_state.group]) ++
    format("~-8.s ~p~n", ["Trace:", State#tracer_state.trace]) ++
    format("~-8.s ~p~n", ["MQueue:", MQueue]) ++
    color_by_pid(self(), format("~" ++ integer_to_list(length(Title) + (64 * 2) + 4) ++ "c~n", [$-])),
  io:put_chars(user, S0).

-spec init_mon_info_tbls() -> ok.
init_mon_info_tbls() ->
  EtsAttrs = [set, public, named_table, {keypos, 1},
    {write_concurrency, true}],
  ets:new(?MON_INFO_ETS_NAME, EtsAttrs),
  ets:new(?MON_INFO_INV_ETS_NAME, EtsAttrs),
  ok.

-spec get_mon_info() -> list().
get_mon_info() ->
  ets:tab2list(?MON_INFO_ETS_NAME).

-spec get_mon_info(MonPid :: pid()) -> Info :: tuple().
get_mon_info(MonPid) ->
  case ets:lookup(?MON_INFO_ETS_NAME, MonPid) of
    [] ->
      undefined;
    [Info] ->
      Info
  end.

-spec set_mon_info(Pid :: pid(), State :: tracer_state()) -> State :: tracer_state().
set_mon_info(Pid, State = #tracer_state{group = Group, trace = Trace}) ->
  MonPid = self(),
  Info = {MonPid, maps:keys(Group), lists:reverse(Trace)},

  % Insert monitor info in lookup and reverse lookup tables.
  ets:insert(?MON_INFO_ETS_NAME, Info),
  ets:insert(?MON_INFO_INV_ETS_NAME, {Pid, MonPid}),
  State.

-spec get_mon_info_rev() -> list().
get_mon_info_rev() ->
  ets:tab2list(?MON_INFO_INV_ETS_NAME).

-spec get_mon_info_rev(Pid :: pid()) -> Info :: tuple() | undefined.
get_mon_info_rev(Pid) ->
  case get_proc_mon(Pid) of
    undefined ->
      undefined;
    MonPid ->
      get_mon_info(MonPid)
  end.

-spec get_proc_mon(Pid :: pid()) -> MonPid :: pid() | undefined.
get_proc_mon(Pid) ->
  case ets:lookup(?MON_INFO_INV_ETS_NAME, Pid) of
    [] ->
      undefined;
    [{_, MonPid}] ->
      MonPid
  end.

-endif.