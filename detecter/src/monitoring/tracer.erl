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
-export([start/4, stop/0]).

-ifdef(TEST).
-export([get_mon_info/0, get_mon_info/1, set_trc_info/2]).
-export([get_mon_info_rev/0, get_mon_info_rev/1]).
-export([get_proc_mon/1]).
-endif.


%%% Internal callbacks.
-export([root/5, tracer/6]).

%%% Types.
-export_type([parent/0, a_mode/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Two modes a tracer can exist in. In direct mode, the tracer receives events
%% directly from the process(es) it tracers; in priority mode, the tracer
%% may still be receiving events directly from the process(es) it traces, but
%% must nevertheless first analyze those events forwarded to it by other tracers
%% (a.k.a. priority events) before it can then analyse the trace events it
%% receives directly from the processes it traces.
-define(T_MODE_DIRECT, direct).
-define(T_MODE_PRIORITY, priority).

-define(A_MODE_INTERNAL, internal).
-define(A_MODE_EXTERNAL, external).


%% TODO: What are these? Probably tables used to keep counts used in the ETS table? We'll see
%% TODO: I think they are used in testing to keep the association between tracers and processes.
-ifdef(TEST).
-define(MON_INFO_ETS_NAME, mon_state).
-define(MON_INFO_INV_ETS_NAME, mon_inv_state).
-endif.

%% Maintains the count of the events seen by the tracer.
-record(stats, {
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
%%   {@item `traced'}
%%   {@desc Processes that are traced by the tracer with their traced mode.}
%%   {@item `mfa_spec'}
%%   {@desc Function that determines whether an analyzer is associated with a
%%          MFA whose process instantiation needs to be monitored.}
%%   {@item `a_mode'}
%%   {@desc Analysis mode determining whether the analysis is conducted
%%          internally in tracer processes or externally in independent
%%          analyzer processes.
%%   }
%%   {@item `trace'}
%%   {@desc List of trace events seen by the tracer.}
%%   {@item `stats'}
%%   {@desc Counts of the events seen by the tracer.}
%% }
-record(state, {
  routes = #{} :: routes(),
  traced = #{} :: traced(),
  mfa_spec = fun({_, _, _}) -> undefined end :: analyzer:mfa_spec(),
  a_mode = ?A_MODE_EXTERNAL :: a_mode(),
  trace = [] :: list(),
  stats = #stats{} :: stats()
}).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type t_mode() :: ?T_MODE_PRIORITY | ?T_MODE_DIRECT.

-type a_mode() :: ?A_MODE_INTERNAL | ?A_MODE_EXTERNAL.

-type state() :: #state{}.

-type routes() :: #{pid() => pid()}.

-type traced() :: #{pid() => t_mode()}.

-type stats() :: #stats{}.

-type parent() :: pid() | self.

-type analyzer() :: pid() | self | undefined.

-type detach() :: {detach, PidRtr :: pid(), PidS :: pid()}.

-type routed(Msg) :: {route, PidRtr :: pid(), Msg}.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Starts and instruments the top-level system process with the root
%%      tracer.
%%
%% {@params
%%   {@name PidS}
%%   {@desc PID of the top-level system process.}
%%   {@name MfaSpec}
%%   {@desc Function that determines whether an analyzer is associated with a
%%          MFA whose process instantiation needs to be monitored.
%%   }
%%   {@name AMode}
%%   {@desc Analysis mode determining whether the analysis is conducted
%%          internally in tracer processes or externally in independent
%%          analyzer processes.
%%   }
%%   {@name Parent}
%%   {@desc PID of supervisor that is linked to tracer process or `self'.}
%% }
%%
%% {@returns PID of root tracer process.}
-spec start(PidS, MfaSpec, AMode, Parent) -> pid()
  when
  PidS :: pid(),
  MfaSpec :: analyzer:mfa_spec(),
  AMode :: a_mode(),
  Parent :: parent().
start(PidS, MfaSpec, AMode, Parent)
  when is_pid(PidS), is_function(MfaSpec, 1),
  is_pid(Parent); Parent =:= self ->

%%  ?exec_if_test(io:format("TESTING~n"), io:format("NOT TESTING~n")), % TODO: Reenable this!
  ?exec_if_test(
    % Create and initialize tracer-process mapping ETS tables.
    init_mon_info_tbls(), ok
  ),

  % Start root tracer.
  Starter = self(),
  spawn(?MODULE, root, [PidS, MfaSpec, AMode, Starter, Parent]).

%% @doc Performs cleanup.
%%
%% {@returns `ok'.}
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

%% @doc Starts tracing the specified top-level system process.
%%
%% {@params
%%   {@name PidS}
%%   {@desc PID of the top-level system process.}
%%   {@name MfaSpec}
%%   {@desc Function that determines whether an analyzer is associated with a
%%          MFA whose process instantiation needs to be monitored.
%%   }
%%   {@name AMode}
%%   {@desc Analysis mode determining whether the analysis is conducted
%%          internally in tracer processes or externally in independent
%%          analyzer processes.
%%   }
%%   {@name Starter}
%%   {@desc PID of the process launching the tracer.}
%%   {@name Parent}
%%   {@desc PID of supervisor to be linked to tracer processes or `self' if no
%%          supervision is required.
%%   }
%% }
%%
%% {@par To prevent the loss of trace events, the root tracer blocks after it
%%       starts tracing `PidS' and waits until it is ack'ed by `Starter'.
%% }
%%
%% {@returns Does not return.}
-spec root(PidS, MfaSpec, AMode, Starter, Parent) -> no_return()
  when
  PidS :: pid(),
  MfaSpec :: analyzer:mfa_spec(),
  AMode :: a_mode(),
  Starter :: pid(),
  Parent :: parent().
root(PidS, MfaSpec, AMode, Starter, Parent) ->

  ?INFO("Started ROOT tracer ~w for top-level process ~w.", [self(), PidS]),

  % Start tracing top-level system process. Trace events from the top-level
  % process and its children will be deposited in the mailbox.
  true = trace_lib:trace(PidS),

  % Syn with starter process and wait for ack. This syn allows the parent to
  % complete its setup before resuming the root tracer.
  util:syn(Starter),

  % Initialize state. Root system process ID is added to the empty set of
  % processes traced by the root tracer.
  State = #state{
    traced = add_proc(PidS, ?T_MODE_DIRECT, #{}), mfa_spec = MfaSpec,
    a_mode = AMode
  },

  ?exec_if_test(
    % Update tracer-process mapping in ETS lookup tables.
    set_trc_info(PidS, State), ok
  ),

  % Start root tracer in 'direct' mode since it has no ancestor. This means
  % that (i) never has to detach from a router tracer, and (ii) no trace events
  % are routed to it. The root tracer has no analyzer associated with it either.
  loop(?T_MODE_DIRECT, State, undefined, Parent).

%% @doc Starts tracing the specified system process.
%%
%% {@params
%%   {@name PidS}
%%   {@desc PID of the top-level system process.}
%%   {@name PidT}
%%   {@desc PID of the router tracer to detach from.}
%%   {@name AnlFun}
%%   {@desc Analysis function that is applied to trace events to determine their
%%          correct or incorrect sequence.
%%   }
%%   {@name MfaSpec}
%%   {@desc Function that determines whether an analyzer is associated with a
%%          MFA whose process instantiation needs to be monitored.
%%   }
%%   {@name AMode}
%%   {@desc Analysis mode determining whether the analysis is conducted
%%          internally in tracer processes or externally in independent
%%          analyzer processes.
%%   }
%%   {@name Parent}
%%   {@desc PID of supervisor to be linked to tracer processes or `self' if no
%%          supervision is required.
%%   }
%% }
%%
%% {@returns Does not return.}
-spec tracer(PidS, PidT, AnlFun, MfaSpec, AMode, Parent) -> no_return()
  when
  PidS :: pid(),
  PidT :: pid(),
  AnlFun :: analyzer:monitor(),
  MfaSpec :: analyzer:mfa_spec(),
  AMode :: a_mode(),
  Parent :: parent().
tracer(PidS, PidT, AnlFun, MfaSpec, AMode, Parent) ->

  % Create new analyzer to which trace events will be dispatched.
  Analyzer = new_analyzer(AnlFun, Parent, AMode),

  % Detach system process from router tracer. The detach command notifies the
  % router tracer that it is now in charge of collecting trace events for this
  % system process directly.
  detach(PidS, PidT),

  % Initialize tracer state. Detached system process ID is added to the empty
  % set of processes traced by this tracer.
  State = #state{
    traced = add_proc(PidS, ?T_MODE_PRIORITY, #{}), mfa_spec = MfaSpec,
    a_mode = AMode
  },

  ?exec_if_test(
    % Update tracer-process mapping in ETS lookup tables.
    set_trc_info(PidS, State), ok
  ),

  % Start tracer in 'priority' mode since it *has* an ancestor. This means that
  % (i) this tracer has detached from, or is in the process of detaching from
  % the router tracer, (ii) the router tracer potentially needs to route trace
  % events to it before this tracer can, in turn, collect trace events for
  % system processes directly. This tracer can transition to 'direct' mode *only
  % if all* the system processes it traces, i.e., those in the traced processes
  % map have been marked as detached (or said map is empty, in which case
  % 'detach' commands are simply discarded).
  loop(?T_MODE_PRIORITY, State, Analyzer, Parent).


%%% ----------------------------------------------------------------------------
%%% Private helper functions (Trace event routing and analysis).
%%% ----------------------------------------------------------------------------

%% @doc Main tracer loop that routes, collects and analyzes trace events.
%%
%% {@params
%%   {@name TMode}
%%   {@desc Tracer mode.}
%%   {@name State}
%%   {@desc Tracer state.}
%%   {@name Analyzer}
%%   {@desc PID of independent trace event analyzer process if the analysis is
%%          external, `self' if the analysis is internal, or `undefined' if no
%%          analysis is performed.
%%   }
%%   {@name Parent}
%%   {@desc PID of supervisor to be linked to tracer processes or `self' if no
%%          supervision is required.
%%   }
%% }
%%
%% {@par When {@mono `Analyzer' =/= `self'}, the tracer creates an independent
%%       analyzer process to which it dispatches trace events for asynchronous
%%       analysis. The tracer and analyzer processes are not linked. When
%%       {@mono `Analyzer' == `self'}, trace events are analyzed internally by
%%       the tracer.
%% }
%% {@par Tracers operate in one of two modes: `direct' or `priority'. In
%%       `direct' mode, a tracer collects trace events for the system process or
%%       processes it traces directly. In `priority' mode, the tracer has to
%%       wait for the router tracer to route trace events to it. Routed priority
%%       trace events necessarily temporally precede any other event the tracer
%%       comes across in the future, and therefore, must be handled first. Only
%%       when these are handled, and a corresponding detach command has been
%%       issued by the tracer for {@emph each} of the system processes it traces
%%       can it transition to `direct' mode.
%% }
%%
%% <p>The contents of the routing table are managed by the receive handlers of
%% the `spawn' and `stp' events. The `spawn' event adds new entries into the
%% routing table only if a tracer (and by extension, a monitor) can be created
%% for the spawned process. This is decided based on the MFA in the `PropLst'
%% dictionary. If no tracer will be created (since the MFA in the `spawn' event
%% is not contained in the `PropLst' dictionary) no entry is added in the
%% routing table. Entries are removed from the routing table once a `stp' event
%% is received. The event `stp' serves to notify the ancestor tracer that the
%% local tracer is subscribed to its local trace, and therefore, requires no
%% further message routing.</p>
%%
%% {@returns Does not return.}
-spec loop(TMode, State, Analyzer, Parent) -> no_return()
  when
  TMode :: t_mode(),
  State :: state(),
  Analyzer :: analyzer(),
  Parent :: parent().
loop(?T_MODE_PRIORITY, State = #state{}, Analyzer, Parent) ->

  % Analyzer reference must be one of PID or the atom 'self' when the tracer is
  % in 'priority' mode. It cannot be 'undefined': this is only allowed for the
  % root tracer which is *always* started in 'direct' mode, never 'priority'.
  ?assert(is_pid(Analyzer) or (Analyzer =:= self), "Analyzer reference is not PID | self"),
%%  ?exec_if_test(show_state(State, ?MODE_PRIORITY), ok),

  % When in 'priority' mode, the tracer dequeues *only* routed messages. These
  % can be either (i) priority trace events, or (ii) 'detach' commands. Invalid
  % junk messages left in the mailbox to be disposed of by the tracer when in
  % 'direct' mode (see receive clause _Other in loop/5 function for 'direct').
  receive
    Msg = {route, PidRtr, Cmd} when element(1, Cmd) =:= detach ->
      ?TRACE("(*) Dequeued command ~w routed from router tracer ~w.", [Msg, PidRtr]),

      % Routed 'detach' command. It may be forwarded to the next hop, but if
      % not possible (no entry in routing map), is handled by the tracer.
      % Handling 'detach' may cause the tracer to transition to 'direct' mode.
      State0 = handle_detach(State, Msg, Analyzer, Parent),
      loop(?T_MODE_PRIORITY, State0, Analyzer, Parent);

    Msg = {route, PidRtr, Evt} when element(1, Evt) =:= trace ->
      ?TRACE("(*) Dequeued trace event ~w routed from router tracer ~w.", [Msg, PidRtr]),

      % Routed trace event. It may be forwarded to the next hop, but if not
      % possible (no entry in routing map), is handled by the tracer.
      State0 = handle_event(?T_MODE_PRIORITY, State, Msg, Analyzer, Parent),
      loop(?T_MODE_PRIORITY, State0, Analyzer, Parent)
  end;

loop(?T_MODE_DIRECT, State = #state{}, Analyzer, Parent) ->
%%  ?exec_if_test(show_state(State, ?MODE_DIRECT), ok),

  % When in 'direct' mode, the tracer dequeues both direct and routed messages.
  % These can be (i) trace events collected directly, (ii) 'detach' commands
  % received directly from descendant tracers, (iii) routed trace events, or
  % (iv) routed 'detach' commands. Invalid junk messages are dequeued by the
  % tracer as well.
  receive
    Evt when element(1, Evt) =:= trace ->
      ?TRACE("(o) Obtained trace event ~w.", [Evt]),

      % Direct trace event. It may be forwarded to the next hop, but if not
      % possible (no entry in routing map), is handled by the tracer.
      State0 = handle_event(?T_MODE_DIRECT, State, Evt, Analyzer, Parent),
      loop(?T_MODE_DIRECT, State0, Analyzer, Parent);

    Cmd when element(1, Cmd) =:= detach ->
      ?TRACE("(o) Signalled by command ~w from ~w.", [Cmd, element(2, Cmd)]),

      % Non-routed 'detach' command. Non-routed 'detach' commands are sent
      % by one specific descendant tracer to signal a system process detach.
      % The tracer reacts by routing the 'detach' command to the next hop, thus
      % designating it as the *router tracer* for that detach and associated
      % descendant tracer process that issued the 'detach'.
      State0 = route_detach(State, Cmd, Analyzer, Parent),
      loop(?T_MODE_DIRECT, State0, Analyzer, Parent);

    Msg = {route, PidRtr, Evt} when element(1, Evt) =:= trace ->
      ?TRACE("(o) Dequeued forwarded trace event ~w from router tracer ~w.",
        [Msg, PidRtr]),

      % Routed trace event. Since a routed trace event must *always* be handled
      % by a tracer when in 'priority' mode, the only possible option is for the
      % tracer to forward the event to the next hop.
      State0 = forwd_event(State, Msg),
      loop(?T_MODE_DIRECT, State0, Analyzer, Parent);

  %% TODO: May be incorporated in the above clause of forwd_routed.
    Msg = {route, PidRtr, Cmd} when element(1, Cmd) =:= detach ->
      ?TRACE("(o) Dequeued forwarded detach command ~w from router tracer ~w.",
        [Msg, PidRtr]),

      % Routed 'detach' command. Since a routed 'detach' command must *always*
      % be handled by a tracer when in 'priority' mode, the only possible option
      % is for the tracer to forward the command to the next hop.
      State0 = forwd_detach(State, Msg, Analyzer, Parent),
      loop(?T_MODE_DIRECT, State0, Analyzer, Parent);

    _Other ->

      % Invalid or unexpected message. Error out as a sanity check indicating
      % the presence of a bug in the algorithm.
      ?ERROR("(o) Dequeued invalid message ~w.", [_Other]),
      error(invalid_event)
  end.

%% @doc Processes trace events in direct and priority modes.
%%
%% {@params
%%   {@name TMode}
%%   {@desc Tracer mode.}
%%   {@name State}
%%   {@desc Tracer state.}
%%   {@name Msg}
%%   {@desc Direct or routed trace event message to route, analyze or forward.}
%%   {@name Analyzer}
%%   {@desc PID of independent trace event analyzer process if the analysis is
%%          external, `self' if the analysis is internal, or `undefined' if no
%%          analysis is performed.
%%   }
%%   {@name Parent}
%%   {@desc PID of supervisor to be linked to tracer processes or `self' if no
%%          supervision is required.
%%   }
%% }
%%
%% {@returns Updated tracer state.}
-spec handle_event(TMode, State, Msg, Analyzer, Parent) -> state()
  when
  TMode :: t_mode(),
  State :: state(),
  Msg :: event:evm_event() | routed(event:evm_event()),
  Analyzer :: analyzer(),
  Parent :: parent().
handle_event(?T_MODE_DIRECT, State, Evt = {trace, PidSrc, spawn, PidTgt, _}, Analyzer, Parent) ->
  do_handle(PidSrc, State,
    fun _Route(PidT) ->

      % Route trace event to next hop. Routing of a 'spawn' events result in the
      % addition of a tracer-process mapping in the tracer routing map.
      route(PidT, Evt),
      State#state{
        routes = add_route(PidTgt, PidT, State#state.routes)
      }
    end,
    fun _Instr() ->

      % Analyze trace event.
      analyze(Analyzer, Evt),

      % Instrument tracer and update processed trace event count.
      State0 = instr(?T_MODE_DIRECT, State, Evt, self(), Parent),
      State1 = upd_state(State0, Evt),

      ?exec_if_test(
        % Update tracer-process mapping in ETS lookup tables.
        set_trc_info(PidTgt, State1), State1
      )
    end);
handle_event(?T_MODE_DIRECT, State, Evt = {trace, PidSrc, exit, _}, Analyzer, Parent) ->
  do_handle(PidSrc, State,
    fun _Route(PidT) ->

      % Route trace event to next hop.
      route(PidT, Evt),
      State
    end,
    fun _Clean() ->

      % Analyze trace event.
      analyze(Analyzer, Evt),

      % Remove terminated system process from traced processes map and update
      % processed trace event count.
      State0 = State#state{
        traced = del_proc(PidSrc, State#state.traced)
      },
      State1 = upd_state(State0, Evt),

      ?exec_if_test(
        % Update tracer-process mapping in ETS lookup tables.
        set_trc_info(PidSrc, State1), State1
      ),

      % Check whether tracer can be terminated.
      try_gc(State1, Analyzer, Parent)
    end);
handle_event(?T_MODE_DIRECT, State, Evt, Analyzer, _) when
  element(3, Evt) =:= send; element(3, Evt) =:= 'receive';
  element(3, Evt) =:= spawned ->
  PidSrc = element(2, Evt),
  do_handle(PidSrc, State,
    fun _Route(PidT) ->

      % Route trace event to next hop.
      route(PidT, Evt),
      State
    end,
    fun _Analyze() ->

      % Analyze trace event.
      analyze(Analyzer, Evt),

      % Update processed trace event count.
      State0 = upd_state(State, Evt),

      ?exec_if_test(
        % Update tracer-process mapping in ETS lookup tables.
        set_trc_info(PidSrc, State0), State0
      )
    end);

handle_event(?T_MODE_PRIORITY, State, Rtd = {route, PidRtr, Evt = {trace, PidSrc, spawn, PidTgt, _}}, Analyzer, Parent) ->
  do_handle(PidSrc, State,
    fun _Forwd(PidT) ->

      % Forward trace event to next hop. Forwarding of 'spawn' events result in
      % the addition of a tracer-process mapping in the tracer routing map.
      forwd(PidT, Rtd),
      State#state{
        routes = add_route(PidTgt, PidT, State#state.routes)
      }
    end,
    fun _Instr() ->

      % In 'priority' mode, analyzer cannot be undefined. An undefined analyzer
      % is set only for the root tracer that always operates in 'direct' mode.
      ?assert((Analyzer =/= undefined) and
        (is_pid(Analyzer) or (Analyzer =:= self)),
        format("Analyzer cannot be undefined for tracer ~w", [self()])
      ),

      % Analyze trace event.
      analyze(Analyzer, Evt),

      % Instrument tracer and update processed trace event count.
      State0 = instr(?T_MODE_PRIORITY, State, Evt, PidRtr, Parent),
      State1 = upd_state(State0, Evt),

      ?exec_if_test(
        % Update tracer-process mapping in ETS lookup tables.
        set_trc_info(PidTgt, State1), State1
      )
    end);
handle_event(?T_MODE_PRIORITY, State, Rtd = {route, _PidRtr, Evt = {trace, PidSrc, exit, _}}, Analyzer, Parent) ->
  do_handle(PidSrc, State,
    fun _Forwd(PidT) ->

      % Forward trace event to next hop.
      forwd(PidT, Rtd),
      State
    end,
    fun _Clean() ->

      % In 'priority' mode, analyzer cannot be undefined. An undefined analyzer
      % is set only for the root tracer that always operates in 'direct' mode.
      ?assert((Analyzer =/= undefined) and
        (is_pid(Analyzer) or (Analyzer =:= self)),
        format("Analyzer cannot be undefined for tracer ~w", [self()])
      ),

      % Analyze trace event.
      analyze(Analyzer, Evt),

      % Remove terminated system process from traced processes map and update
      % processed trace event count.
      State0 = State#state{
        traced = del_proc(PidSrc, State#state.traced)
      },
      State1 = upd_state(State0, Evt),

      ?exec_if_test(
        % Update tracer-process mapping in ETS lookup tables.
        set_trc_info(PidSrc, State1), State1
      ),

      % Check whether tracer can be terminated.
      try_gc(State1, Analyzer, Parent)
    end);
handle_event(?T_MODE_PRIORITY, State, Rtd = {route, _PidRtr, Evt}, Analyzer, _) when
  element(3, Evt) =:= send; element(3, Evt) =:= 'receive'; element(3, Evt) =:= spawned ->
  PidSrc = element(2, Evt),
  do_handle(PidSrc, State,
    fun _Forwd(PidT) ->

      % Forward trace event to next hop.
      forwd(PidT, Rtd),
      State
    end,
    fun _Analyze() ->

      % In 'priority' mode, analyzer cannot be undefined. An undefined analyzer
      % is set only for the root tracer that always operates in 'direct' mode.
      ?assert((Analyzer =/= undefined) and
        (is_pid(Analyzer) or (Analyzer =:= self)),
        format("Analyzer cannot be undefined for tracer ~w", [self()])
      ),

      % Analyze trace event.
      analyze(Analyzer, Evt),

      % Update processed trace event count.
      State0 = upd_state(State, Evt),

      ?exec_if_test(
        % Update tracer-process mapping in ETS lookup tables.
        set_trc_info(PidSrc, State0), State0
      )
    end);

handle_event(_, State, Msg, _, _) ->
  ?WARN("Discarding trace event ~p.", [Msg]),
  State.

%% @doc Instruments a system process with a new tracer or adds the system
%% process under the current tracer.
%%
%% {@params
%%   {@name TMode}
%%   {@desc Tracer mode.}
%%   {@name State}
%%   {@desc Tracer state.}
%%   {@name Evt}
%%   {@desc Trace event carrying the information of the new system process.}
%%   {@name PidT}
%%   {@desc PID of the router tracer to detach from.}
%%   {@name Parent}
%%   {@desc PID of supervisor to be linked to tracer processes or `self' if no
%%          supervision is required.
%%   }
%% }
%%
%% {@returns Updated tracer state.}
-spec instr(TMode, State, Evt, PidT, Parent) -> state()
  when
  TMode :: t_mode(),
  State :: state(),
  Evt :: event:evm_event(),
  PidT :: pid(),
  Parent :: parent().
instr(Mode, State, {trace, _, spawn, PidTgt, Mfa = {_, _, _}}, PidT, Parent)
  when Mode =:= ?T_MODE_PRIORITY; Mode =:= ?T_MODE_DIRECT ->

  % Check whether a new tracer needs to be instrumented for the system process.
  % This check is performed against the instrumentation map which contains the
  % MFAs that require tracing.
  case (State#state.mfa_spec)(Mfa) of
    undefined ->
      ?TRACE("No tracer found for process ~w; adding to own traced processes map.", [PidTgt]),

      % A new tracer is not required. In 'priority' mode, the tracer handles a
      % 'spawn' trace event of an existing system process that is being traced
      % by another (router) tracer. Therefore, send a 'detach' command to the
      % router tracer to signal that the system process will be transferred
      % under the tracer that collects trace events directly from the process.
      % In 'direct' mode, this 'spawn' event is collected directly from the
      % newly created system process (i.e., event is not routed) *and* a new
      % tracer is not created. Consequently, there is no 'detach' to perform.
      if Mode =:= ?T_MODE_PRIORITY -> detach(PidTgt, PidT); true -> ok end,

      % New system process is added to the traced processes map under the
      % tracer. The process is marked with the tracer mode it was added in.
      State#state{
        traced = add_proc(PidTgt, Mode, State#state.traced)
      };
    {ok, MonFun} ->

      % A new tracer is required. In 'priority' mode, the tracer handles a
      % 'spawn' trace event of an existing system process that is being traced
      % by another (router) tracer. In direct mode, this 'spawn' event is
      % collected directly from the newly-created system process (i.e., the
      % event is not routed). However, a new tracer needs to be created.
      % Consequently, in both priority and direct modes, a 'detach' is required
      % so signal to the router tracer that the system process will be
      % transferred under the new tracer that collects events directly from this
      % process. When in 'priority' mode, the router tracer will never be a
      % parent of the new tracer, but an ancestor; in 'direct' mode the router
      % tracer to whom the 'detach' is sent will always be a (direct) parent of
      % the new tracer. The 'detach' command is sent by the new tracer, see
      % trace/5 for details.
      %
      % Note that the new system process is not added to the traced processes
      % map of the tracer since it is being traced by the new tracer.
      Args = [PidTgt, PidT, MonFun, State#state.mfa_spec, State#state.a_mode, Parent],
      PidT0 = spawn(?MODULE, tracer, Args),

      ?INFO("Instrumenting tracer ~w on MFA ~w for process ~w.", [PidT0, Mfa, PidTgt]),

      % Create a new process-tracer mapping in the routes map to enable the
      % tracer to forward events to the next hop. The next hop is, in fact, the
      % newly-created tracer.
      State#state{
        routes = add_route(PidTgt, PidT0, State#state.routes)
      }
  end.

%% @doc Routes detach commands to the next hop.
%%
%% {@params
%%   {@name State}
%%   {@desc Tracer state.}
%%   {@name Cmd}
%%   {@desc Detach command to route.}
%%   {@name Analyzer}
%%   {@desc PID of independent trace event analyzer process if the analysis is
%%          external, `self' if the analysis is internal, or `undefined' if no
%%          analysis is performed.
%%   }
%%   {@name Parent}
%%   {@desc PID of supervisor to be linked to tracer processes or `self' if no
%%          supervision is required.
%%   }
%% }
%%
%% {@par The function should be used in `direct' mode.}
%% {@par Detach commands {@emph must} be routed when the tracer is in `direct'
%%       mode.
%% }
%%
%% {@returns Updated tracer state, or does not return if tracer is garbage
%%           collected.
%% }
-spec route_detach(State, Cmd, Analyzer, Parent) -> state() | no_return()
  when
  State :: state(),
  Cmd :: detach(),
  Analyzer :: analyzer(),
  Parent :: parent().
route_detach(State, Cmd = {detach, PidT, PidTgt}, Analyzer, Parent) ->
  do_handle(PidTgt, State,
    fun _Route(PidT) ->

      % Route 'detach' command to next hop. Commands to be routed are sent by
      % one specific descendant tracer to signal a system process detach. This
      % means that the entry for that process-tracer mapping in all other tracer
      % routing maps becomes redundant, for the tracer sending the detach
      % command can now collect trace events directly for the system process in
      % question. As a result, it no longer relies on trace events being routed
      % to it for that particular system process. The process-tracer mapping is
      % removed from the routing map. All tracers in subsequent hops handle the
      % routed 'detach' command analogously.
      route(PidT, Cmd),
      State0 = State#state{routes = del_route(PidTgt, State#state.routes)},

      % Check whether tracer can be terminated.
      try_gc(State0, Analyzer, Parent)
    end,
    fun _Fail() ->

      % *Invariant*: For the command to be routed, a corresponding entry in
      % the routing map must exist. This entry should have been created by the
      % tracer when it handled the 'spawn' event of the process whose command in
      % question is being routed.

      % *Violation*: If this case is reached, such an entry does not exist and
      % the command cannot be routed to the next hop, and it must have been
      % sent to the tracer by mistake.
      ?assert(false, format("Detach command sent from tracer ~w not expected", [PidT]))
    end).

%% @doc Forwards detach commands to the next hop.
%%
%% {@params
%%   {@name State}
%%   {@desc Tracer state.}
%%   {@name Rtd}
%%   {@desc Routed message to forward.}
%%   {@name Analyzer}
%%   {@desc PID of independent trace event analyzer process if the analysis is
%%          external, `self' if the analysis is internal, or `undefined' if no
%%          analysis is performed.
%%   }
%%   {@name Parent}
%%   {@desc PID of supervisor to be linked to tracer processes or `self' if no
%%          supervision is required.
%%   }
%% }
%%
%% {@par The function should be used in `direct' mode.}
%% {@par When a tracer is in `priority' mode, it can either handle or forward
%%       `detach' commands. This command enables a tracer to transition from
%%       `priority' mode to `direct' mode only once all the processes in the
%%        traced processes map are marked as detached by the tracer; this
%%        procedure can only be accomplished by the tracer when it handles the
%%        command for the particular process being detached. When, the command
%%        cannot be handled by the tracer, it is forwarded to the next hop, see
%%        {@link handle_detach/4}. A `detach' is not designed to be handled by
%%        tracers when in `direct' mode, and {@emph must} always be forwarded.
%% }
%%
%% {@returns Updated tracer state, or does not return if tracer is garbage
%%           collected.}
-spec forwd_detach(State, Rtd, Analyzer, Parent) -> state() | no_return()
  when
  State :: state(),
  Rtd :: routed(detach()),
  Analyzer :: analyzer(),
  Parent :: parent().
forwd_detach(State, Msg = {route, _, {detach, _PidT, PidTgt}}, Analyzer, Parent) ->
  do_handle(PidTgt, State,
    fun _Forwd(PidT) ->

      % Forward 'detach' command to next hop. Forwarding of 'detach' commands
      % results in the removal of a process-tracer mapping in the tracer
      % routing map.
      forwd(PidT, Msg),
      State0 = State#state{
        routes = del_route(PidTgt, State#state.routes)
      },

      % ** Harmless race condition **
      % There are cases when a 'detach' command sent by a tracer to the router
      % tracer for a particular process is routed back by the latter tracer
      % *after* the process in question has been removed from the traced
      % processes map of the tracer sending the 'detach' command. This happens
      % when the 'exit' trace event of the process is handled by the tracer.
      % Note that such a scenario necessarily arises when the tracer is in
      % 'priority' mode; it cannot arise when the tracer is in 'direct' mode
      % simply because the tracer has not yet handled the 'detach' command, and
      % is therefore, still in 'priority' mode.
      %
      % In cases where there are no processes to trace (all 'exit' events have
      % been handled), the associated tracer is garbage collected, and the
      % 'detach' command is forwarded to a non-existent tracer.
      %
      % Both of these cases are harmless, and the tracer choreography is still
      % sound. These are two examples where this situation might occur:
      %
      % Suppose a system consists of two processes, P, Q and R. Q is forked by
      % P, and R is forked by Q, and the whole execution completes before any
      % tracer is created. The trace is thus: 'fork(P, Q).fork(Q, R).exit(R)'.
      % 1. The first case is where Q and R are traced by separate tracers. When
      %    the root tracer processes 'fork(P, Q)', it creates a tracer TQ for Q.
      %    TQ is in 'priority' mode with process Q in its traced processes map.
      %    TQ sends a 'detach' command to the root tracer that in turn, routes
      %    back to TQ. Next, the root tracer routes 'fork(Q, R)' to TQ which
      %    handles it to create TR for R. TR is in 'priority' mode with process
      %    R in its traced processes map. TQ sends a 'detach' command to the
      %    root tracer. Finally, the root tracer routes 'exit(R)' to TR via
      %    tracer TQ. When TR handles 'exit(R)' removes the entry for R from its
      %    traced processes map and terminates (it has an empty routing map).
      %    Eventually, the 'detach' command for process R is forwarded by TQ to
      %    the non-existent tracer TR. The command for R is not processed, but
      %    the tracer choreography remains sound.

      % Check whether tracer can be terminated.
      try_gc(State0, Analyzer, Parent)
    end,
    fun _DetachOfATerminatedProcessNoLongerInTheTracedProcessesMapOfTracer() ->

      % ** Harmless race condition (continued from the previous case) **
      % 2. The second case is where Q and R are traced by the same tracer. When
      %    the root tracer processes 'fork(P, Q)', it creates a tracer TQR. TQR
      %    is in 'priority' mode with process Q in its traced processes map. TQR
      %    sends a 'detach' command for process Q to the root tracer. Next, the
      %    root tracer routes 'fork(Q, R)' to TQR which handles it to add
      %    process R to its traced process map too. TQR sends a 'detach' command
      %    for R to the root tracer. Finally, the root tracer routes 'exit(R)'
      %    to TQR which handles it by removing R from its traced processes map.
      %    Note that process R is removed while R is marked as 'priority'.
      %    Eventually, the 'detach' command for process Q is routed by the
      %    root tracer to TQR, which results in Q being marked as 'direct' in
      %    the traced processes map of TQR. Meanwhile, note that the routing map
      %    in TQR is empty. Since all the processes in the traced processes map
      %    (only Q) are marked as 'direct', TQR switched to 'direct' mode. Right
      %    after, the 'detach' command for process R reaches TQR. Now, this
      %    command cannot be routed, since the routing map is empty, and must
      %    therefore be handled (i.e., this case). By design, there is nothing
      %    to handle, since process R has already been removed from the traced
      %    processes map of TQR. Similar to case 1 above, the command for R is
      %    not processed, albeit for a different reason, but the tracer
      %    choreography remains sound.
      ?TRACE("Routed 'detach' command handled for (already) terminated process ~w.", [PidTgt]),
      State
    end).

%% @doc Handles detach commands or forwards them to the next hop.
%%
%% {@params
%%   {@name State}
%%   {@desc Tracer state.}
%%   {@name Rtd}
%%   {@desc Routed message to forward.}
%%   {@name Analyzer}
%%   {@desc PID of independent trace event analyzer process if the analysis is
%%          external, `self' if the analysis is internal, or `undefined' if no
%%          analysis is performed.
%%   }
%%   {@name Parent}
%%   {@desc PID of supervisor to be linked to tracer processes or `self' if no
%%          supervision is required.
%%   }
%% }
%%
%% {@par The function should be used in `priority' mode.}
%% {@par Detach commands can be either handled {@emph or} forwarded when the
%%       tracer is in `priority' mode.
%% }
%%
%% {@returns Updated tracer state, or does not return if tracer is garbage
%%           collected.}
-spec handle_detach(State, Rtd, Analyzer, Parent) -> state() | no_return()
  when
  State :: state(),
  Rtd :: routed(detach()),
  Analyzer :: analyzer(),
  Parent :: parent().
handle_detach(State, Rtd = {route, _, {detach, Self, PidTgt}}, Analyzer, Parent) ->
  do_handle(PidTgt, State,
    fun _Relay(PidT) ->

      % Forward 'detach' command to next hop. Forwarding of 'detach' commands
      % results in the removal of a process-tracer mapping in the tracer
      % routing map.
      forwd(PidT, Rtd),
      State0 = State#state{
        routes = del_route(PidTgt, State#state.routes)
      },

      % Check whether tracer can be terminated.
      try_gc(State0, Analyzer, Parent)
    end,
    fun _CheckIfCanTransitionToDirectMode() ->

      % *Invariant*: A 'detach' command that is not forwarded must always be
      % handled by the tracer in 'priority' mode. This means that the tracer
      % handling the command must be the same tracer that sent the `detach'
      % command to the routed tracer.

      % *Violation*: The PID of the sending tracer embedded inside the 'detach'
      % command is not the same as self().
      ?assertEqual(self(), Self, format("Tracer ~w is not equal to ~w for detach command", [Self, self()])),

      % A 'detach' command is interpreted as an end-of-trace marker for the
      % particular process being traced. Update the entry for the detached
      % system process in the traced processes map by switching it from
      % 'priority' to direct'. The tracer now collects events for said process
      % directly from the trace.
      State0 = State#state{
        traced = upd_proc(PidTgt, ?T_MODE_DIRECT, State#state.traced)
      },

      % Check whether tracer can transition to 'direct' mode. This is possible
      % only if all processes in the traced processes map are marked 'direct'.
      case can_detach(State0) of
        true ->

          ?TRACE("Tracer ~w switched to ~w mode.", [self(), ?T_MODE_DIRECT]),
          loop(?T_MODE_DIRECT, State0, Analyzer, Parent);
        false ->
          State0
      end
    end).

%% @doc Forwards routed trace events.
%%
%% {@params
%%   {@name State}
%%   {@desc Tracer state.}
%%   {@name Rtd}
%%   {@desc Routed message to forward.}
%% }
%%
%% {@par The function should be used in `direct' mode.}
%% {@par When a tracer is in `priority' mode, it can either handle or forward
%%       trace events. Handling enables the tracer to analyze the event. When
%%       the event cannot be handled by the tracer, it is forwarded to the next
%%       hop, see {@link handle_event/5}, `priority' mode. A trace event is not
%%       designed to be handled by tracers when in `direct' mode, and {@emph
%%       must} always be forwarded.
%% }
%%
%% {@returns Update tracer state.}
% TODO: Can this be tightened? It can, but the result is more cryptic (IN FUTURE).
-spec forwd_event(State, Rtd) -> state()
  when
  State :: state(),
  Rtd :: routed(event:evm_event()).
forwd_event(State, Rtd = {route, _, {trace, PidSrc, spawn, PidTgt, _}}) ->
  do_handle(PidSrc, State,
    fun _Forwd(PidT) ->

      % Forward trace event to next hop. Forwarding of 'spawn' events results in
      % the addition of a process-tracer mapping in the tracer routing map for
      % the child process.
      forwd(PidT, Rtd),
      State#state{
        routes = add_route(PidTgt, PidT, State#state.routes)
      }
    end,
    fun _Fail() ->

      % *Invariant*: For the event to be forwarded, a corresponding entry in the
      % routing map must exist. This entry should have been created by the
      % tracer when it handled the 'spawn' event of the process whose event in
      % question is being forwarded.

      % *Violation*: If this case is reached, such an entry does not exist and
      % the event cannot be forwarded. This means that the event must be handled
      % by the tracer, but this goes against the assumption that the event
      % should have been handled by the tracer when in 'priority' mode.
      ?assert(false, format("Routed trace event ~w cannot be handled while in ~s mode", [Rtd, ?T_MODE_DIRECT]))
    end);
forwd_event(State, Rtd = {route, _, Evt}) ->
  PidSrc = element(2, Evt),
  do_handle(PidSrc, State,
    fun _Forwd(PidT) ->

      % Forward trace event to next hop.
      forwd(PidT, Rtd),
      State
    end,
    fun _Fail() ->

      % *Invariant*: For the event to be forwarded, a corresponding entry in the
      % routing map must exist. This entry should have been created by the
      % tracer when it handled the 'spawn' event of the process whose event in
      % question is being forwarded.

      % *Violation*: If this case is reached, such an entry does not exist and
      % the event cannot be forwarded. This means that the event must be handled
      % by the tracer, but this goes against the assumption that the event
      % should have been handled by the tracer when in 'priority' mode.
      ?assert(false, format("Routed trace event ~w cannot be handled while in ~s mode", [Rtd, ?T_MODE_DIRECT]))
    end).


%%% ----------------------------------------------------------------------------
%%% Private helper functions (Tracer utility).
%%% ----------------------------------------------------------------------------

-spec do_handle(PidSrc, State, OnForward, OnHandle) -> term()
  when
  PidSrc :: pid(),
  State :: state(),
  OnForward :: fun((NextHop :: pid()) -> term()),
  OnHandle :: fun(() -> term()).
do_handle(PidSrc, #state{routes = Routes, traced = Traced}, OnForward, OnHandle)
  when is_function(OnHandle, 0), is_function(OnForward, 1) ->

  % TODO: Update docs.
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
  ?assert((not maps:is_key(PidSrc, Routes)) or not maps:is_key(PidSrc, Traced)),

  case maps:get(PidSrc, Routes, undefined) of
    undefined ->
      OnHandle();
    NextHop ->
      OnForward(NextHop)
  end.

%% @doc Determines whether a tracer can transition to `direct' mode.
%%
%% {@params
%%   {@name State}
%%   {@desc Tracer state.}
%% }
%%
%% {@returns `true' if the tracer can transition to `direct' mode, otherwise
%%           false.
%% }
-spec can_detach(State :: state()) -> boolean().
can_detach(#state{traced = Traced}) ->
  length(lists:filter(
    fun(?T_MODE_DIRECT) -> false; (_) -> true end, maps:values(Traced)
  )) =:= 0.

-spec detach(PidS :: pid(), PidT :: pid()) -> Detach :: detach().
detach(PidS, PidT) ->

  % TODO: Update docs.
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

%% @doc Terminates the tracer when the routes and traced processes maps are
%% empty.
%%
%% {@params
%%   {@name State}
%%   {@desc Tracer state.}
%%   {@name Analyzer}
%%   {@desc PID of independent trace event analyzer process if the analysis is
%%          external, `self' if the analysis is internal, or `undefined' if no
%%          analysis is performed.
%%   }
%%   {@name Parent}
%%   {@desc PID of supervisor to be linked to tracer processes or `self' if no
%%          supervision is required.
%%   }
%% }
%%
%% {@returns Updated tracer state, or does not return if tracer is garbage
%%           collected.}
-spec try_gc(State, Analyzer, Parent) -> state() | no_return()
  when
  State :: state(),
  Analyzer :: analyzer(),
  Parent :: parent().
try_gc(State = #state{traced = Traced, routes = Routes}, Analyzer, Parent) when
  map_size(Traced) =:= 0, map_size(Routes) =:= 0 ->

  % Stop analyzer. Depending on the analyzer type, the analysis of events ceases
  % immediately when the tracer terminates, or resumes independently in the
  % analyzer process until completed. In the latter case, the tracer does not
  % block or wait for the analysis to finish.
  stop_analyzer(Analyzer),

  % Get tracer tag.
  Tag = if Analyzer =:= undefined -> root; true -> tracer end,

  % Dump tracer state.
  ?exec_if_test(show_state(State, Tag), ok),

  % If available, link to parent process. Providing the parent =/= self and it
  % traps exits, it receives trace event stats collected by the tracer.
  if is_pid(Parent) -> link(Parent); true -> ok end,

  ?TRACE("Garbage collecting tracer ~w.", [self()]),
  exit({garbage_collect, {Tag, State#state.stats}});

try_gc(State = #state{}, _, _) ->
  State.

%% @doc Routes the specified trace event or command.
%%
%% {@params
%%   {@name PidT}
%%   {@desc PID of destination tracer process.}
%%   {@name Msg}
%%   {@desc Trace event or command to route.}
%% }
%%
%% {@returns Routed message.}
-spec route(PidT, Msg) -> routed(event:evm_event()) | routed(detach())
  when
  PidT :: pid(),
  Msg :: event:evm_event() | detach().
route(PidT, Msg) when element(1, Msg) =:= trace; element(1, Msg) =:= detach ->
  ?TRACE("Tracer ~w routing ~w to next hop ~w.", [self(), Msg, PidT]),
  PidT ! {route, self(), Msg}.

%% @doc Forwards the specified trace event or command.
%%
%% {@params
%%   {@name PidT}
%%   {@desc PID of destination tracer process.}
%%   {@name Rtd}
%%   {@desc Routed trace event or command to forward.}
%% }
%%
%% {@returns Forwarded message.}
-spec forwd(PidT, Rtd) -> routed(event:evm_event()) | routed(detach())
  when
  PidT :: pid(),
  Rtd :: routed(event:evm_event()) | routed(detach()).
forwd(PidT, Rtd) when element(1, Rtd) =:= route ->
  ?TRACE("Tracer ~w forwarding ~w to next hop ~w.", [self(), Rtd, PidT]),
  PidT ! Rtd.


%%% ----------------------------------------------------------------------------
%%% Private helper functions (State management).
%%% ----------------------------------------------------------------------------

%% @doc Adds the specified system process to the traced processes map.
%%
%% {@params
%%   {@name PidS}
%%   {@desc PID of system process to add.}
%%   {@name TMode}
%%   {@desc Tracer mode.}
%%   {@name Traced}
%%   {@desc Traced processes map.}
%% }
%%
%% {@returns Updated traced processes map.}
-spec add_proc(PidS, TMode, Traced) -> traced()
  when
  PidS :: pid(),
  TMode :: t_mode(),
  Traced :: traced().
add_proc(PidS, TMode, Traced) ->
  ?assertNot(maps:is_key(PidS, Traced),
    format("Process ~w should not be in traced processes map", [PidS])
  ),
  Traced#{PidS => TMode}.

%% @doc Deletes the specified system process from the traced processes map.
%%
%% {@params
%%   {@name PidS}
%%   {@desc PID of system process to delete.}
%%   {@name Traced}
%%   {@desc Traced processes map.}
%% }
%%
%% {@returns Updated traced processes map.}
-spec del_proc(PidS :: pid(), Traced :: traced()) -> traced().
del_proc(PidS, Traced) ->
  % ?assert(maps:is_key(PidS, Traced), % TODO: Commented this for now since might be a potential bug.
  %   format("Process ~w must exist when deleting", [PidS])),

  % TODO: Is it always the case that a process must exist before deleting?
  % TODO: Well for sure, a process is deleted when an exit is processed.
  % TODO: Think long and hard about this, and come up with an execution
  % TODO: interleaving where this can occur.

  % This log was left here. It will cause the deletion to fail when there is
  % no process inside the map, since maps:get/2 requires the key to exist. But
  % I left it here as a reminder on what the bug or interleaving scenario I
  % did not cater for ot think about could be.
  ?TRACE("Process ~w deleted from traced processes map while in ~w mode.", [PidS, maps:get(PidS, Traced)]),
  maps:remove(PidS, Traced).

%% @doc Updates the specified system process in the traced processes map with
%%      the new mode.
%%
%% {@params
%%   {@name PidS}
%%   {@desc PID of system process to update.}
%%   {@name NewMode}
%%   {@desc New process mode.}
%%   {@name Traced}
%%   {@desc Traced processes map.}
%% }
%%
%% {@par No update is performed if `PidS' does not exist.}
%%
%% {@returns Updated traced processes map.}
-spec upd_proc(PidS, NewMode, Traced) -> traced()
  when
  PidS :: pid(),
  NewMode :: t_mode(),
  Traced :: traced().
upd_proc(PidS, NewMode, Traced) ->

  % It might be possible for the process to be updated to have been already
  % removed from the traced processes map. This happens when the system process
  % exits before the 'detach' command reaches the tracer, in which case there
  % is no process to mark from 'priority' to 'direct'. In this case, the traced
  % processes map is left unmodified.
  case maps:is_key(PidS, Traced) of
    true ->
      Traced#{PidS := NewMode}; % Only update existing value, otherwise fail.
    false ->
      ?TRACE("Process ~w not updated since not in traced processes map.", [PidS]),
      Traced
  end.

%% @doc Adds a new process-tracer mapping to the routes map.
%%
%% {@params
%%   {@name PidS}
%%   {@desc PID of system process for which future trace events will be routed
%%          or forwarded.
%%   }
%%   {@name PidT}
%%   {@desc PID of tracer process to which future trace events from `PidS' are
%%          forwarded.
%%   }
%%   {@name Routes}
%%   {@desc Routes map.}
%% }
%%
%% {@returns Updated routes map.}
-spec add_route(PidS :: pid(), PidT :: pid(), Routes :: routes()) -> routes().
add_route(PidS, PidT, Routes) ->
  ?assertNot(maps:is_key(PidS, Routes),
    format("Process ~w already in routes map", [PidS])
  ),
  Routes#{PidS => PidT}.

%% @doc Deletes the existing process-tracer mapping from the routes map.
%%
%% {@params
%%   {@name PidS}
%%   {@desc PID of system process for which mapping is to be deleted.}
%%   {@name Routes}
%%   {@desc Routes map.}
%% }
%%
%% {@returns Updated routes map.}
-spec del_route(PidS :: pid(), routes()) -> routes().
del_route(PidS, Routes) ->
  ?assert(maps:is_key(PidS, Routes),
    format("Process ~w not in routes map", [PidS])
  ),
  maps:remove(PidS, Routes).

%% @doc Updates the processed event counts for the specified event.
%%
%% {@params
%%   {@name Stats}
%%   {@desc Trace event counts.}
%%   {@name Evt}
%%   {@desc Event to increment.}
%% }
%%
%% {@returns Updated event counts.}
-spec upd_stats(Stats, Evt) -> stats()
  when
  Stats :: stats(),
  Evt :: tuple().
upd_stats(Stats = #stats{cnt_spawn = Cnt}, {trace, _, spawn, _, _}) ->
  Stats#stats{cnt_spawn = Cnt + 1};
upd_stats(Stats = #stats{cnt_exit = Cnt}, {trace, _, exit, _}) ->
  Stats#stats{cnt_exit = Cnt + 1};
upd_stats(Stats = #stats{cnt_send = Cnt}, {trace, _, send, _, _}) ->
  Stats#stats{cnt_send = Cnt + 1};
upd_stats(Stats = #stats{cnt_receive = Cnt}, {trace, _, 'receive', _}) ->
  Stats#stats{cnt_receive = Cnt + 1};
upd_stats(Stats = #stats{cnt_spawned = Cnt}, {trace, _, spawned, _, _}) ->
  Stats#stats{cnt_spawned = Cnt + 1};
upd_stats(Stats = #stats{cnt_other = Cnt}, Evt) when element(1, Evt) =:= trace ->
  Stats#stats{cnt_other = Cnt + 1}.

%% @doc Updates the tracer state for the specified event.
%%
%% {@params
%%   {@name State}
%%   {@desc Tracer state.}
%%   {@name Evt}
%%   {@desc Event for which the state is to be updated.}
%% }
%%
%% {@returns Updated tracer state.}
-spec upd_state(State, Evt) -> state()
  when
  State :: state(),
  Evt :: event:evm_event().
upd_state(State = #state{trace = _Trace, stats = Stats}, Event) ->
  State0 = State#state{stats = upd_stats(Stats, Event)},
  ?exec_if_test(State0#state{trace = [Event | _Trace]}, State0).


%%% ----------------------------------------------------------------------------
%%% Private helper functions (Event analysis).
%%% ----------------------------------------------------------------------------

%% @doc Creates a new internal or external trace event analyzer that is
%%      initialized with the specified analysis function.
%%
%% {@params
%%   {@name AnlFun}
%%   {@desc Analysis function that is applied to trace events to determine their
%%          correct or incorrect sequence.
%%   }
%%   {@name Parent}
%%   {@desc PID of supervisor to be linked to tracer processes or `self' if no
%%          supervision is required.
%%   }
%%   {@name AMode}
%%   {@desc Analysis mode.}
%% }
%%
%% {@returns PID of external analyzer process or `self' if the analyzer is
%%           embedded in the tracer process itself.
%% }
-spec new_analyzer(AnlFun, Parent, AMode) -> pid() | self
  when
  AnlFun :: analyzer:monitor(),
  Parent :: parent(),
  AMode :: a_mode().
new_analyzer(AnlFun, _, ?A_MODE_INTERNAL) ->
  true = analyzer:embed(AnlFun),
  ?TRACE("Embedded new analyzer in tracer ~w.", [self()]),
  self;
new_analyzer(AnlFun, Parent, ?A_MODE_EXTERNAL) ->
  Pid = analyzer:start(AnlFun, Parent),
  ?TRACE("Forked new independent analyzer ~w for tracer ~w.", [Pid, self()]),
  Pid.

%% @doc Stops the embedded or process-based analyzer, if defined.
%%
%% {@params
%%   {@name Analyzer}
%%   {@desc Analyzer to stop.}
%% }
%%
%% {@returns `ok' to indicate success.}
-spec stop_analyzer(Analyzer :: analyzer()) -> ok.
stop_analyzer(undefined) ->
  ok;
stop_analyzer(self) ->
  ok;
stop_analyzer(PidA) when is_pid(PidA) ->
  analyzer:stop(PidA),
  ok.

%% @doc Analyses the specified trace event.
%%
%% {@params
%%   {@name Analyzer}
%%   {@desc Analyzer to use.}
%%   {@name Evt}
%%   {@desc Trace event to analyze.}
%% }
%%
%% {@returns Analyzed event.}
-spec analyze(Analyzer, Evt) -> Evt
  when
  Analyzer :: analyzer(),
  Evt :: event:evm_event().
analyze(undefined, Evt) ->

  % An undefined analyzer is set only for the root tracer that always operates
  % in 'direct' mode. Any other tracer besides the root must have an analyzer
  % that is either external (PID) or internal (self).
  Evt;
analyze(self, Evt) ->
  ?TRACE("Tracer ~w analyzing event ~w internally.", [self(), Evt]),
  analyzer:do_monitor(Evt, fun(_Verdict) -> ok end),
  Evt;
analyze(PidA, Evt) when is_pid(PidA) ->
  ?TRACE("Tracer ~w ~s event ~w to ~w for external analysis.",
    [self(),
      case is_process_alive(PidA) of true -> "dispatched"; _ -> "did not dispatch" end,
      Evt, PidA]),
  PidA ! Evt.


%%% ----------------------------------------------------------------------------
%%% Private helper functions (Misc).
%%% ----------------------------------------------------------------------------

%% @doc Returns a character list that represents the data formatted according to
%%      the specified format flags.
%%
%% {@params
%%   {@name Fmt}
%%   {@desc String formatting.}
%%   {@name Args}
%%   {@desc Arguments to pass to the formatting.}
%% }
%%
%% {@returns Formatted character list.}
-spec format(Fmt :: string(), Args :: list()) -> String :: string().
format(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).


%%% ----------------------------------------------------------------------------
%%% Testing.
%%% ----------------------------------------------------------------------------

-ifdef(TEST).

-spec color_by_pid(Pid :: pid(), Text :: string()) -> iolist().
color_by_pid(Pid, Text) when is_pid(Pid) ->
  {_, N, _} = pid_tokens(Pid),
  Code = N rem 255,
  ["\e[38;5;", integer_to_list(Code), "m", Text, "\e[0m"].

-spec pid_tokens(Pid :: pid()) ->
  {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
pid_tokens(Pid) when is_pid(Pid) ->
  pid_tokens(lists:reverse(pid_to_list(Pid)), 1, 0, {}).

-spec pid_tokens(PidChars, Fact, N, Elements) ->
  {non_neg_integer(), non_neg_integer(), non_neg_integer()}
  when
  PidChars :: string(),
  Fact :: pos_integer(),
  N :: non_neg_integer(),
  Elements :: tuple().
pid_tokens([], _, N, Elements) ->
  erlang:insert_element(1, Elements, N);
pid_tokens([$< | Tokens], Fact, N, Elements) ->
  pid_tokens(Tokens, Fact, N, Elements);
pid_tokens([$> | Tokens], Fact, N, Elements) ->
  pid_tokens(Tokens, Fact, N, Elements);
pid_tokens([Token | Tokens], Fact, N, Elements) when Token >= $0, Token =< $9 ->
  pid_tokens(Tokens, Fact * 10, N + (Token - $0) * Fact, Elements);
pid_tokens([$. | Tokens], _, N, Elements) ->
  pid_tokens(Tokens, 1, 0, erlang:insert_element(1, Elements, N)).

-spec show_state(State :: state(), Data :: any()) -> ok.
show_state(State, Data) ->
  {messages, MQueue} = erlang:process_info(self(), messages),

  Title = format("(~p) Tracer ~w", [Data, self()]),
  S0 = color_by_pid(self(), format("~n~64c[ ~s ]~64c~n", [$-, Title, $-])) ++
    format("~-8.s ~p~n", ["Routes:", State#state.routes]) ++
    format("~-8.s ~p~n", ["Traced:", State#state.traced]) ++
    format("~-8.s ~p~n", ["Stats:", State#state.stats]) ++
    format("~-8.s ~p~n", ["Trace:", lists:reverse(State#state.trace)]) ++
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

-spec set_trc_info(Pid :: pid(), State :: state()) -> State :: state().
set_trc_info(Pid, State = #state{traced = Traced, trace = Trace}) ->
  MonPid = self(),
  Info = {MonPid, maps:keys(Traced), lists:reverse(Trace)},

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