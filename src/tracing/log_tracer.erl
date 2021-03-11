%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Asynchronous tracing that monitors log file for trace events.
%%%
%%% {@par The module provides a file-based tracing implementation that actively
%%%       monitors files of trace events to read and dispatch them to allocated
%%%       tracers. Source log files are structured as {@emph one} trace event
%%%       per line; these events describe the execution trace of a single
%%%       process. The tracer handles interleaved trace events written to the
%%%       file by multiple processes, and guarantees that the sequential order
%%%       of events from the point of view of individual processes is respected.
%%% }
%%% {@par This tracing implementation is forward-only, meaning that it cannot
%%%       revisit previously processed lines in the source log file. It also
%%%       does not assume that the log file (and traces within) is complete.
%%%       although it a complete log file can be easily processed. Internally,
%%%       the tracer employs a polling mechanism (see {@link log_poller} to
%%%       monitor the log file, similar to what is done by `tail' in *nix
%%%       systems.
%%% }
%%%
%%% {@sect Log file format}
%%%
%%% {@par For the format of individual trace events, see {@link log_eval}.}
%%% {@par Log files are structured as one trace event description per line:
%%% ```
%%% fork(<0.17.0>, <0.19.0>, @{mod_p_4, start, []@})
%%% init(<0.19.0>, <0.17.0>, @{mod_p_4, start, []@})
%%% send(<0.19.0>, <0.11.0>, msg_c)
%%% exit(<0.19.0>, kill_p_4)
%%% fork(<0.11.0>, <0.13.0>, @{mod_p_2, start, []@})
%%% init(<0.13.0>, <0.11.0>, @{mod_p_2, start, []@})
%%% recv(<0.13.0>, msg_a)
%%% send(<0.11.0>, <0.13.0>, msg_a)
%%% recv(<0.11.0>, msg_c)
%%% exit(<0.11.0>, kill_p_1)
%%% fork(<0.13.0>, <0.17.0>, @{mod_p_3, start, []@})
%%% send(<0.13.0>, <0.17.0>, msg_b)
%%% init(<0.17.0>, <0.13.0>, @{mod_p_3, start, []@})
%%% recv(<0.17.0>, msg_b)
%%% exit(<0.17.0>, kill_p_3)
%%% exit(<0.13.0>, kill_p_2)
%%% '''
%%% }
%%%
%%% {@par This log file describes the behavior of four concurrent processes,
%%%       P<sub>1</sub> ({@lt}0.11.0{@gt}), P<sub>2</sub> ({@lt}0.13.0{@gt}),
%%%       P<sub>3</sub> ({@lt}0.17.0{@gt}) and P<sub>4</sub> ({@lt}0.19.0{@gt}).
%%%       The behaviour of these processes is causally related: P<sub>1</sub>
%%%       forks P<sub>2</sub>, which in its turn, forks P<sub>3</sub>, with the
%%%       latter finally forking P<sub>4</sub>. Notwithstanding their causality,
%%%       the respective trace events of these processes appear in a different
%%%       order than one would expect due to their interleaved execution. This
%%%       reordering may also arise when each process is competing for the
%%%       {@emph shared write lock} that needs to be acquired before the trace
%%%       event can be written to the log file by the process.
%%% }
%%% {@par Processes P<sub>1</sub> to P<sub>4</sub> are sequential entities. One
%%%       should distinguish between the relative ordering of trace events for
%%%       each process, as opposed to the global ordering of events as reported
%%%       in the log file. The log tracer implementation expects the trace event
%%%       order for one particular process to be logically consistent with the
%%%       behaviour of the process when this is considered in isolation. To
%%%       illustrate, the interleaved global trace in the example above can be
%%%       projected into the following four traces:
%%% }
%%% {@dl
%%%   {@term Trace P<sub>1</sub> (note that the {@mono init} trace event for
%%%          P<sub>1</sub> is missing from the log file):
%%%   }
%%%   {@desc {@ol
%%%            {@item {@mono fork({@lt}0.11.0{@gt}, {@lt}0.13.0{@gt}, @{mod_p_2, start, []@})}}
%%%            {@item {@mono send({@lt}0.11.0{@gt}, {@lt}0.13.0{@gt}, msg_a)}}
%%%            {@item {@mono recv({@lt}0.11.0{@gt}, msg_c)}}
%%%            {@item {@mono exit({@lt}0.11.0{@gt}, kill_p_1)}}
%%%   }}
%%%   {@term Trace P<sub>2</sub>:}
%%%   {@desc {@ol
%%%            {@item {@mono init({@lt}0.13.0{@gt}, {@lt}0.11.0{@gt}, @{mod_p_2, start, []@})}}
%%%            {@item {@mono recv({@lt}0.13.0{@gt}, msg_a)}}
%%%            {@item {@mono fork({@lt}0.13.0{@gt}, {@lt}0.17.0{@gt}, @{mod_p_3, start, []@})}}
%%%            {@item {@mono send({@lt}0.13.0{@gt}, {@lt}0.17.0{@gt}, msg_b)}}
%%%            {@item {@mono exit({@lt}0.13.0{@gt}, kill_p_2)}}
%%%   }}
%%%   {@term Trace P<sub>3</sub>:}
%%%   {@desc {@ol
%%%            {@item {@mono fork({@lt}0.17.0{@gt}, {@lt}0.19.0{@gt}, @{mod_p_4, start, []@})}}
%%%            {@item {@mono init({@lt}0.17.0{@gt}, {@lt}0.13.0{@gt}, @{mod_p_3, start, []@})}}
%%%            {@item {@mono recv({@lt}0.17.0{@gt}, msg_b)}}
%%%            {@item {@mono exit({@lt}0.17.0{@gt}, kill_p_3)}}
%%%   }}
%%%   {@term Trace P<sub>4</sub>:}
%%%   {@desc {@ol
%%%            {@item {@mono init({@lt}0.19.0{@gt}, {@lt}0.17.0{@gt}, @{mod_p_4, start, []@})}}
%%%            {@item {@mono send({@lt}0.19.0{@gt}, {@lt}0.11.0{@gt}, msg_c)}}
%%%            {@item {@mono exit({@lt}0.19.0{@gt}, kill_p_4)}}
%%%   }}
%%% }
%%% {@par The projected traces for P<sub>1</sub> to P<sub>4</sub> are valid. For
%%%       instance, {@mono exit} events, that signal process termination are,
%%%       never followed by any other event: this would constitute unsound
%%%       behaviour where the process first terminated and then performed other
%%%       actions. Likewise, {@mono init} events (when present in the trace),
%%%       are the first to be exhibited, indicating that a process must be
%%%       initialized (or created) before it can perform any other action. To
%%%       summarize, trace events for individual process are totally ordered,
%%%       whereas a global view of the events for all processes generally yields
%%%       a partial ordering.
%%% }
%%%
%%% {@sect Automatic tracer allocation}
%%%
%%% {@par To be filled later.}
%%%
%%% {@sect Tracer guarantees}
%%%
%%% {@par To be filled later.}
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
-module(log_tracer).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([start/1, start/2, stop/0]).
-export([trace/1, clear/1, preempt/1]).
-export([post_event/1, post_events/1]).

-ifdef(TEST).
-export([get_tracer/1, get_backlog/0]).
-endif.

%%% Callbacks.
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

%%% Types.
-export_type([event/0]).

%%% Implemented behaviors.
-behavior(gen_server).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Allocated tracers table name.
-define(ETS_ALLOC_NAME, allocs).

%% Relates a process PID with its allocated tracer PID.
%%
%% {@dl
%%   {@item `tracee'}
%%   {@desc PID of process being traced.}
%%   {@item `tracer'}
%%   {@desc PID of tracer process.}
%% }
-record(alloc, {
  tracee :: pid(),
  tracer :: pid()
}).

%% Guard macro that validates the trace event tuple.
-define(is_event(Event), tuple_size(Event) >= 2,
  is_atom(element(1, Event)), is_pid(element(2, Event))).

%% Guard macro that validates the timeout.
-define(is_timeout(Ms), is_integer(Ms), Ms >= 0).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type event() :: {delay, Ms :: timeout(), Event :: event:int_event()}.
%% Delayed trace event with data payload.

-type backlog() :: list(event()).
%% Backlog of undelivered trace events.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Configures and starts the tracer.
%%
%% {@params
%%   {@name File}
%%   {@desc Full path of log file containing the trace event descriptions.}
%% }
%%
%% {@returns PID of the tracer process.}
-spec start(File :: file:filename()) ->
  {ok, Pid :: pid()} | {error, {already_started, Pid :: pid()}}.
start(File) ->
  start(File, []).

%% @doc Configures and starts the tracer.
%%
%% {@params
%%   {@name File}
%%   {@desc Full path of log file containing the trace event descriptions.}
%%   {@name Opts}
%%   {@desc Configuration options. No options are currently supported.}
%% }
%%
%% {@returns PID of the tracer process.}
-spec start(File :: file:filename(), Opts :: proplists:proplist()) ->
  {ok, Pid :: pid()} |
  {error, {already_started, Pid :: pid()}}.
start(File, Opts) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [File], Opts).

%% @doc Stops the tracer.
%%
%% {@returns `ok' to indicate successful termination.}
-spec stop() -> ok | no_return().
stop() ->
  gen_server:stop(?MODULE).

%% @doc Sets the caller process as the tracer for tracee.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc PID of the process to trace.}
%% }
%%
%% {@returns `true' if successful, or `false' if `Tracee' is already being
%%            traced.}
-spec trace(Tracee :: pid()) -> Success :: boolean().
trace(Tracee) when is_pid(Tracee) -> %% Can be asynchronous to speed things up.
  case create_alloc(Tracee, self()) of
    Reply = true ->
      ok = gen_server:cast(?MODULE, update),
      Reply;
    Reply ->
      Reply
  end.

%% @doc Clears the tracer for the specified tracee.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc PID of the process whose tracer is to be cleared.}
%% }
%%
%% {@returns `true' to acknowledge that `Tracee' is no longer being traced.}
-spec clear(Tracee :: pid()) -> true.
clear(Tracee) when is_pid(Tracee) ->
  delete_alloc(Tracee).

%% @doc Sets the caller process as the new tracer for tracee.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc PID of the process to trace.}
%% }
%%
%% {@returns `true' if successful, or `false' if `Tracee' is not being traced.}
-spec preempt(Tracee :: pid()) -> Success :: boolean().
preempt(Tracee) when is_pid(Tracee) ->

  % This implementation of preempt (i) updates the allocated tracers table
  % atomically, and subsequently, (ii) issues a synchronous message to the log
  % tracer process. This forces preempt to return *only* once a reply from the
  % log tracer process is received by the caller of preempt. This design decision
  % was taken so that delayed trace events `delayed by <Ms>' cause a blocking
  % effect within the preempt call, and correspondingly, on the tracer making
  % the invocation. For this scheme to work, it is paramount that the unblock
  % request issued by preempt to the log tracer is processed in the same manner
  % as other trace events posted by the log poller process, i.e., in the order
  % there were received in the log tracer's mailbox. Note that this implies that
  % calls to preempt that *do* update the allocated tracers table, i.e.,
  % `update_alloc/2' returns true, are unblocked after an event dispatch by the
  % log tracer is fully completed, assuming, of course, an event dispatch is
  % underway. We saw fit not to block preempt in case `update_alloc/2' returns
  % false. This was simply a design decision we took that has nothing to do with
  % avoiding or solving race conditions. In fact, there is a second alternative.
  %
  % This would involve first performing the blocking call followed by the update
  % on the allocated tracers table. The slight downside with this approach is
  % that preempt blocks regardless of whether the update to the allocated
  % tracers table is successful (i.e., a preemption of an existing process) or
  % not (i.e., a preemption of a non-existent process).
  ?TRACE("Data in table BEFORE: ~p~n~n", [ets:tab2list(?ETS_ALLOC_NAME)]),
  case update_alloc(Tracee, self()) of
    true ->
      Ref = erlang:make_ref(),

      ?TRACE("Successfully preempted tracer ~p for tracee ~p.", [self(), Tracee]),
      ?TRACE("Data in table AFTER: ~p~n~n", [ets:tab2list(?ETS_ALLOC_NAME)]),

      % Wait on tracer to give signal to unblock. We want to block indefinitely
      % in case there are delays (e.g. events with delays) > 5000ms, which is
      % the default timeout for gen_server:call/2.
      {unblocked, Ref} = gen_server:call(?MODULE, {unblock, Ref}, infinity),
      true;
    false ->

      ?WARN("Could not preempt tracer ~p for tracee ~p.", [self(), Tracee]),
      ?TRACE("Data in table AFTER: ~p~n~n", [ets:tab2list(?ETS_ALLOC_NAME)]),
      false
  end.

%% @doc Posts a trace event description to the tracer.
%%
%% {@params
%%   {@name Event}
%%   {@desc Trace event description to post.}
%% }
%%
%% {@returns `ok' to acknowledge success.}
-spec post_event(Event :: event()) -> ok.
post_event(Event) ->
  gen_server:call(?MODULE, {post, Event}, infinity).

%% @doc Posts a list of trace event descriptions to the tracer.
%%
%% {@params
%%   {@name Events}
%%   {@desc List of trace event descriptions to post.}
%% }
%%
%% {@returns List of `ok' atoms for each acknowledged post.}
-spec post_events(Events :: list(event())) -> list(ok).
post_events([]) ->
  [];
post_events([Event | Events]) ->
  [post_event(Event) | post_events(Events)].


%%% ----------------------------------------------------------------------------
%%% Callbacks.
%%% ----------------------------------------------------------------------------

%% @doc Handles initialization.
%%
%% {@params
%%   {@name Args}
%%   {@desc A single-element list that points to the full path path of log file
%%          containing the trace event descriptions.
%%   }
%% }
%%
%% {@returns Empty state.}
-spec init(Args :: [File :: file:filename()]) -> no_return().
init([File]) ->

  % Create fresh allocations table.
  create_alloc_table(),

  % Start log file poller process.
  log_poller:start_link(File, []),
  {ok, []}.

%% @private Handles termination.
%%
%% {@params
%%   {@name Reason}
%%   {@desc Termination reason.}
%%   {@name Backlog}
%%   {@desc Trace event backlog.}
%% }
%%
%% {@returns `ok' to indicate successful termination.}
-spec terminate(Reason, Backlog) -> ok when
  Reason :: normal | shutdown | {shutdown, term()} | term(),
  Backlog :: backlog().
terminate(Reason, _) ->
  Stop = log_poller:stop(),
  delete_alloc_table(),
  ?DEBUG("Tracer stopped for the following reasons ~p and ~p.", [Reason, Stop]).

%% @doc Handles requests synchronously.
%%
%% {@par The following requests are supported:}
%% {@dl
%%   {@term {@code get_backlog}}
%%   {@desc {@returns Complete trace event backlog to the requesting process.}}
%%   {@term {@code @{post, Event::{@link event()}@}}}
%%   {@desc
%%     {@par Accepts a trace event description to be dispatched to the a tracer
%%           if this is allocated to a tracee. If the trace event could not be
%%           immediately dispatched due to the absence of such an allocation, it
%%           is instead appended to the backlog.
%%     }
%%     {@par If the event dispatch resulted in the automatic allocation of a new
%%           tracer, all of the queued events in the backlog are examined to
%%           determine whether these have become eligible for dispatch in view
%%           of this recent tracer allocation. These are handled accordingly,
%%           should this be the case. Note that this handling may itself
%%           automatically allocate further tracers, and this must be
%%           continually repeated until all the events in the backlog that can
%%           be dispatched have been dispatched, and the tracer is in a stable
%%           state.
%%     }
%%     {@par Processing trace events in the backlog in sequential fashion
%%           ensures that the order in which these were admitted into the
%%           backlog remains preserved, despite the possible dynamic automatic
%%           allocations of new tracers.
%%     }
%%     {@returns `ok' to acknowledge the requesting process.}
%%   }
%%   {@term {@code @{unblock, Ref::{@link reference()}@}}}
%%   {@desc Blocks the caller until the request is processed by the tracer. Used
%%          internally by the {@link preempt/1} implementation, which might
%%          change in the future.
%%          {@returns {@code @{unblocked, Ref@}} to unblock the requesting process.}
%%   }
%% }
-spec handle_call(Request, From, Backlog) ->
  {reply, Backlog, Backlog} |
  {reply, ok, NewBacklog :: backlog()} |
  {reply, {unblocked, Ref :: reference()}, Backlog}
  when
  Request :: get_backlog | {post, Event :: event()} | {unblock, Ref :: reference()},
  From :: {Pid :: pid(), Tag :: reference()},
  Backlog :: backlog().
handle_call(get_backlog, _, Backlog) ->
  {reply, Backlog, Backlog};
handle_call({post, Entry = {delay, Ms, Event}}, _, Backlog) when
  ?is_timeout(Ms), ?is_event(Event) ->

  % Delay dispatching of event to trace event queue by Ms.
  timer:sleep(Ms),
  case dispatch(Entry) of
    false ->

      % Process event has not been dispatched to tracer. Append event to
      % backlog.
      {reply, ok, lists:reverse([Entry | lists:reverse(Backlog)])};

    {true, stable} ->

      % Process event has been dispatched to tracer and no update was made to
      % the allocated tracers table. Since event has been dispatched, do not
      % append it to backlog.
      {reply, ok, Backlog};

    {true, update} ->

      % Process event has been dispatched to tracer, and moreover, the allocated
      % tracers table was updated. This may make events queued in the backlog
      % eligible to being dispatched, and the backlog needs to be processed.
      % Since this processing may itself create additional updates, the
      % processing must be continually repeated until all events that can be
      % dispatched have been dispatched
      NewBacklog = replay_backlog(Backlog),
      {reply, ok, NewBacklog}
  end;
handle_call({unblock, Ref}, _From, Backlog) ->
  {reply, {unblocked, Ref}, Backlog}.

%% @doc Handles requests asynchronously.
%%
%% {@par The following requests are supported:}
%% {@dl
%%   {@term `update'}
%%   {@desc Indicates to the log tracer that a new tracer allocation has been
%%          created externally via the API.
%%     {@returns No reply is returned.}
%%   }
%% }
-spec handle_cast(Request, Backlog) -> {noreply, UpdatedBacklog :: backlog()}
  when
  Request :: update,
  Backlog :: backlog().
handle_cast(update, Backlog) ->

  % A new tracer allocation has been created externally via the API. This may
  % make events queued in the backlog eligible to being dispatched, and the
  % backlog needs to be processed. Since this processing may itself create
  % additional allocations, the processing must be continually repeated until
  % all events that can be dispatched have been dispatched
  {noreply, replay_backlog(Backlog)}.


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% @doc Dispatches trace events from the backlog to tracers.
%%
%% {@params
%%   {@name Backlog}
%%   {@desc Trace event backlog to process.}
%% }
%%
%% {@par Trace events in the backlog are processed sequentially. This ensures
%%       that events are dispatched in the correct order with respect to one
%%       another, corresponding to the order in which these were admitted into
%%       the backlog.
%% }
%% {@par Dispatching `fork' events will result in the automatic allocation of
%%       new tracers (see {@link play_backlog/1}). When this happens, the
%%       sequential processing of the backlog is restarted at the head of the
%%       backlog. This guarantees that any queued trace events for the forked
%%       process that can be now dispatched (in light of the new tracer
%%       allocation) are dispatched before other recent trace events for said
%%       process. This reprocessing of the backlog may, yet again, give rise to
%%       further automatic tracer allocations, which need to be handled as
%%       described. In this manner, this function is reapplied to eliminate from
%%       the backlog, via these intermediate steps, all the trace events that
%%       can be dispatched, to yield a stable backlog.
%% }
%%
%% {@returns Stable backlog.}
-spec replay_backlog(Backlog :: backlog()) -> ProcessedBacklog :: backlog().
replay_backlog(Backlog) ->
  case play_backlog(Backlog) of
    {stable, ProcessedBacklog} ->

      % Backlog has been processed in its entirety with respect to all known
      % entries in the allocated tracers table. All events that were meant to
      % be dispatched have been dispatched.
      ProcessedBacklog;
    {replay, PartiallyProcessedBacklog} ->

      % Backlog processing has been interrupted due to a new entry being created
      % in the tracer allocations table. As this could mean that certain queued
      % events in the backlog are now eligible to being dispatched, reprocess
      % the backlog once again from the beginning.
      replay_backlog(PartiallyProcessedBacklog)
  end.

%% @doc Dispatches events from the the backlog to tracers.
%%
%% {@params
%%   {@name Backlog}
%%   {@desc Trace event backlog to process.}
%% }
%%
%% {@par When examining each trace event in the backlog, one of three scenarios
%%       arises. These are handled thus:
%% }
%% {@ol
%%   {@item Current trace event could not be dispatched since no tracers are
%%          allocated at present. The event is left in the backlog.
%%   }
%%   {@item Current trace event was dispatched since an allocated tracer exists.
%%          The event is removed from the backlog.
%%   }
%%   {@item Current trace event was dispatched since an allocated tracer exists.
%%          The event is removed from the backlog. Additionally, processing the
%%          event resulted in the automatic allocation of a tracer. The only
%%          instance where this is possible is in the case of `fork(P, Q)' trace
%%          events, where the same tracer `T' for process `P' is also allocated
%%          to the forked process `Q'. Processing of the backlog ceases.
%%   }
%% }
%%
%% {@returns Corresponds to the above scenarios:
%%   {@dl
%%     {@term `@{stable, ProcessedBacklog@}'}
%%     {@desc Backlog has been processed, and all events that could be
%%            dispatched (if any) were dispatched. This is the case for 1 and 2.
%%     }
%%     {@term `@{replay, PartiallyProcessedBacklog@}'}
%%     {@desc Backlog processing interrupted due to the automatic allocation of
%%            a tracer. This could potentially render previously examined trace
%%            events in the backlog eligible for dispatch. Consequently, ensuing
%%            events in the backlog cannot be processed until all other previous
%%            events are reexamined to determine whether these can be dispatched
%%            or otherwise. When this is the case, the caller of this function
%%            must re-invoke this function anew, specifying the partially
%%            processed backlog, `PartiallyProcessedBacklog', returned by the
%%            current call (see {@link replay_backlog}). This is the case for 3.
%%     }
%%   }
%% }
-spec play_backlog(Backlog :: backlog()) ->
  {stable, ProcessedBacklog :: backlog()} |
  {replay, PartiallyProcessedBacklog :: backlog()}.
play_backlog([]) ->
  {stable, []};
play_backlog([Entry | Backlog]) ->
  case dispatch(Entry) of
    false ->

      %% TODO: Assert that the size of the ETS table is unchanged.
      %% TODO: Assers that the size of backlog is unchanched.

      % Trace event was not dispatched to tracer. Skip event and process next.
      {Status, Rest} = play_backlog(Backlog),
      {Status, [Entry | Rest]};

    {true, stable} ->

      %% TODO: Assert that the size of the ETS table is unchanged.
      %% TODO: Assers that the size of backlog is smaller by 1.

      % Trace event was dispatched to tracer and no update has been made to
      % allocated tracers table. Remove event from backlog and process next.
      play_backlog(Backlog);

    {true, update} ->

      % TODO: Assert that this is fork.
      % TODO: Assert that size of ETS is greater by 1.
      %% TODO: Assers that the size of backlog is smaller by 1.

      % Trace event was dispatched to tracer and an update has been made to
      % allocated tracers table. As this update may affect subsequent event
      % dispatching for the process in question, stop processing remaining
      % backlog events.
      {replay, Backlog}
  end.

%% @doc Creates the allocated tracers table.
%%
%% {@returns Table name.}
-spec create_alloc_table() -> ?ETS_ALLOC_NAME.
create_alloc_table() ->
  % The default locking is on table-level, allowing only one update
  % operation at a time per table.
  % Table option `write_concurrency' enables locking on a more fine
  % grained level, allowing concurrent update operations. In the current
  % implementation 16 locks per table is used, which results in a probability
  % of 1/16 that two random keys will collide on the same lock.
  % See: http://erlang.org/pipermail/erlang-questions/2012-May/066632.html
  % * `write_concurrency' enables concurrent writes.
  % * `read_concurrency' speeds up concurrent reads.
  EtsAttrs = [set, public, named_table, {keypos, #alloc.tracee},
    {write_concurrency, true}],
  ets:new(?ETS_ALLOC_NAME, EtsAttrs).

%% @doc Deletes the allocated tracers table.
%%
%% {@returns `true' regardless of whether the table exists.}
-spec delete_alloc_table() -> true.
delete_alloc_table() ->
  ets:delete(?ETS_ALLOC_NAME).

%% @doc Creates a new entry into the allocated tracers table.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc PID of the tracee.}
%%   {@name Tracer}
%%   {@desc PID of the tracer to allocate.}
%% }
%% {@par Operation fails if `Tracee' already exists in the table.}
%%
%% {@returns `true' if the creation succeeded, otherwise `false'.}
-spec create_alloc(Tracee :: pid(), Tracer :: pid()) -> Success :: boolean().
create_alloc(Tracee, Tracer) ->
  ets:insert_new(?ETS_ALLOC_NAME, #alloc{tracee = Tracee, tracer = Tracer}).

%% @doc Returns the PID of the allocated tracer for the specified tracee PID.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc PID of the tracee whose tracer is to be returned.}
%% }
%%
%% {@returns PID of the tracer if existent, otherwise `undefined'.}
-spec return_alloc(Tracee :: pid()) -> Tracer :: pid() | undefined.
return_alloc(Tracee) ->
  case ets:lookup(?ETS_ALLOC_NAME, Tracee) of
    [] ->
      undefined;
    [#alloc{tracer = Tracer}] ->
      Tracer
  end.

%% @doc Updates the tracer of the associated tracee.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc PID of the tracee whose tracer is to be updated.}
%%   {@name Tracer}
%%   {@desc New tracer PID.}
%% }
%%
%% {@returns `true' if the update succeeded, otherwise `false'.}
-spec update_alloc(Tracee :: pid(), Tracer :: pid()) -> Success :: boolean().
update_alloc(Tracee, Tracer) ->
  ets:update_element(?ETS_ALLOC_NAME, Tracee, {#alloc.tracer, Tracer}).

%% @doc Deletes the entry identified by tracee from the allocated tracers table.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc PID of the tracee whose entry is to be deleted.}
%% }
%%
%% {@returns `true' regardless of whether the tracee exists.}
-spec delete_alloc(Tracee :: pid()) -> true.
delete_alloc(Tracee) ->
  ets:delete(?ETS_ALLOC_NAME, Tracee).

%% @doc Dispatches the trace event according to the allocated tracers table.
%%
%% {@params
%%   {@name Event}
%%   {@desc Trace event description dispatch.}
%% }
%%
%% {@par Trace events are handled according to their type:}
%% {@dl
%%   {@term `fork(P, Q)'}
%%   {@desc If process `P' is allocated a tracer `T', the event is dispatched to
%%          `T'. As a side-effect, `Q' is allocated `T', so that subsequent
%%          events are dispatched to it. This emulates the automatic tracer
%%          inheritance of forked processes in the EVM tracer. The event is
%%          otherwise left in the backlog if no tracer is allocated to `P'.
%%   }
%%   {@term `exit(P)'}
%%   {@desc If process `P' is allocated a tracer `T', the event is dispatched to
%%          `T'. As a side-effect, the allocation of `T' for `P' is removed.
%%          This emulates the automatic tracer removal for terminated processes
%%          in the EVM tracer. The trace event is otherwise left in the backlog
%%          if no tracer is allocated to `P'.
%%   }
%%   {@term `init(P, Q)', `send(P, Q)', `recv(Q)'}
%%   {@desc If process `P' is allocated a tracer `T', the event is dispatched to
%%          `T'. The event is otherwise left in the backlog if no tracer is
%%          allocated to `P'.
%%   }
%% }
%%
%% {@returns Three outcomes are possible:
%%           {@dl
%%             {@item `false'}
%%             {@desc Trace event could not be dispatched since no tracer is
%%                    allocated.
%%             }
%%             {@item `@{true, stable@}'}
%%             {@desc Trace event has been dispatched. The backlog does not need
%%                    to be revisited to examine events preceding the current
%%                    one.
%%             }
%%             {@item `@{true, update@}'}
%%             {@desc Trace event has been dispatched and a new tracer has been
%%                    automatically allocated. The backlog needs to be revisited
%%                    to determine whether events preceding the current one can
%%                    be dispatched.
%%             }
%%           }
%% }
-spec dispatch(Event :: event()) -> false | {true, stable} | {true, update}.
dispatch(Event = {delay, _, _E = {fork, _, Pid2, _}}) ->
  gen_dispatch(Event,
    fun(Tracer) ->

      ?TRACE("AUTO allocating tracer ~p to forked process ~p (~p).", [Tracer, Pid2, _E]),

      % ** Harmless race condition **
      % To handle automatic tracer inheritance, allocate the tracer to the
      % forked process PID. At this point, it is possible that immediately
      % before this allocation is effected, an external entity allocates the
      % tracer via `trace/1'. This race condition does not impinge on event
      % dispatching, and the invocation to `create_alloc/2' below simply returns
      % `false' instead of true, since the tracer would have already been
      % allocated manually by the caller of `trace/1', rather than automatically
      % by this function.

      % ** Harmless race condition**
      % 1. Auto allocating tracer T1 to forked process P1 does not manage to
      %    call create_alloc in time, and the ETS table has no entry for tracer
      %    T1.
      % 2. Another tracer T2 calls preempt(T1) which returns false since there
      %    is no entry in the ETS table for tracer T1.
      % 3. The entry in the ETS due to the auto allocation is performed for
      %    tracer T1.
      % 4. As a result, the old tracer T1 remains allocated to P1, rather than
      %    T2 being allocated to P1.
      % 5. Is this behavior incorrect according to the EVM tracing?
      % No, because the log tracer can never know which tracer will invoke
      % preempt for which process, and therefore cannot just block
      % indiscriminately each time preempt is invoked. For all we know, the
      % tracer might preempt on a process that is non-existent, which would
      % result in the log tracer blocking indefinitely until that process has an
      % associated trace event that is processed.
      create_alloc(Pid2, Tracer),
      update
    end);
dispatch(Event = {delay, _, _E = {exit, Pid, _}}) ->
  gen_dispatch(Event,
    fun(Tracer) ->

      ?TRACE("Deallocating tracer ~p for exiting process ~p (~p).", [Tracer, Pid, _E]),

      % ** Harmless race condition **
      % Automatically deallocate tracer for terminated PID. As this point, it is
      % possible that immediately before this deallocation is done, an external
      % process deallocates the tracer via `clear/1'. This race condition does
      % not impinge on event dispatching.
      delete_alloc(Pid),
      stable
    end);
dispatch(Entry = {delay, _, _}) ->
  gen_dispatch(Entry, fun(_) -> stable end).

%% @doc Handles event dispatching and invokes the specified callback on success.
%%
%% {@params
%%   {@name Event}
%%   {@desc Trace event to be dispatched.}
%%   {@name OnDispatch}
%%   {@desc Callback accepting a single parameter `Tracer' that is populated
%%          with the allocated tracer PID.
%%   }
%% }
%%
%% {@returns Two outcomes are possible:
%%           {@dl
%%             {@term `false'}
%%             {@desc Trace event could not be dispatched since no tracer is
%%                    allocated.
%%             }
%%             {@term `@{true, Result@}'}
%%             {@desc Trace event has been dispatched, and the result of the
%%                    `OnDispatch' is returned in `Result'.
%%             }
%%           }
%% }
-spec gen_dispatch(Event, OnDispatch) -> Status :: false | {true, any()}
  when
  Event :: event(),
  OnDispatch :: function().
gen_dispatch({delay, _Ms, Event0}, OnDispatch) when is_function(OnDispatch, 1) ->
  Pid = element(2, Event0),
  case return_alloc(Pid) of
    undefined ->

      ?TRACE("No tracer allocated to ~p: placing ~p on backlog.", [Pid, Event0]),

      % No tracer allocated to PID - withhold dispatching.
      false;
    Tracer when is_pid(Tracer) ->

      % Invoke callback for optional pre-processing.
      Return = OnDispatch(Tracer),

      % A tracer is allocated to source PID.
      ?TRACE("Tracer ~p allocated to ~p: dispatching ~p.", [Tracer, Pid, Event0]),

      % ** Implementation note **
      % We could have instead modified the evm_tracer module to add a translator
      % from EVM-specific descriptions to more general intermediate trace event
      % descriptions (e.g. fork instead of the EVM-specific spawn event). This
      % approach would have required introducing an extra translation layer that
      % complicates matters. Instead, we settled for EVM-specific trace events
      % as our default events, and do the translation in the opposite way, i.e,
      % translate from general events to the EVM format before these are
      % dispatched to tracers.
      Tracer ! event:to_evm_event(Event0),

      % Perhaps, we can improve the unblocking of PREEMPTED tracers by
      % unblocking them earlier.
      % For another time since it requires more thinking and study. Problem is
      % that we can only write, and not read and write to preserve atomicity
      % (read and update is not atomic, so we update blindly). Another way would
      % be to keep one dirty flag (boolean). When we preempt we set the dirty
      % flag to true in the ETS, so that the tracer knows to unblock the tracer
      % that called preempt. If the preempt is called by another process on the
      % same tracee, the preempt unblocks the old dirty with an error. We cannot
      % do it as well, since the update_element call is destructive. Will need
      % to think about some other strategy.
      % 1. Unblock dirty.
      % 2. Send msg.
      {true, Return}
  end.


%%% ----------------------------------------------------------------------------
%%% Testing.
%%% ----------------------------------------------------------------------------

-ifdef(TEST).

%% @doc Determines whether tracee is being traced.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc PID of the process to check.}
%% }
%%
%% {@returns PID of the associated tracer or `undefined' if `Tracee' is not
%%           being traced.
%% }
-spec get_tracer(Tracee :: pid()) -> pid() | undefined.
get_tracer(Tracee) when is_pid(Tracee) ->
  return_alloc(Tracee).

%% @doc Fetches the backlog of trace events currently queued for delivery.
%%
%% {@returns Trace event queue.}
-spec get_backlog() -> Backlog :: backlog().
get_backlog() ->
  gen_server:call(?MODULE, get_backlog, infinity).

-endif.