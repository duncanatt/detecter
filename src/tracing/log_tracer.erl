%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Wrapper for the Log file tracing infrastructure.
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
-export([trace/1, clear/1, preempt/1, query/1]).
-export([get_backlog/0, post_event/1, post_events/1]).

%%% Callbacks.
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

%%% Types.
%%-export_type([log/0]).

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

-type backlog() :: list(event()).




%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-spec start(File :: file:filename()) ->
  {ok, Pid :: pid()} |
  {error, {already_started, Pid :: pid()}}.
start(File) ->
  start(File, []).


-spec start(File :: file:filename(), Opts :: proplists:proplist()) ->
  {ok, Pid :: pid()} |
  {error, {already_started, Pid :: pid()}}.
start(File, Opts) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [File], Opts).

-spec stop() -> ok | no_return().
stop() ->
  gen_server:stop(?MODULE).

-spec trace(Tracee :: pid()) -> Success :: boolean().
trace(Tracee) when is_pid(Tracee) -> %% Can be asynchronous to speed things up.
  case create_alloc(Tracee, self()) of
    Reply = true ->
      ok = gen_server:cast(?MODULE, update),
      Reply;
    Reply ->
      Reply
  end.

-spec clear(Tracee :: pid()) -> true.
clear(Tracee) when is_pid(Tracee) ->
  delete_alloc(Tracee).

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


-spec post_event(Event :: log_eval:entry()) -> ok.
post_event(Event) ->
  gen_server:call(?MODULE, {post, Event}, infinity).

-spec post_events(Events :: [log_eval:entry()]) -> [ok].
post_events([]) ->
  [];
post_events([Event | Events]) ->
  [post_event(Event) | post_events(Events)].


% TODO: Is this needed? (probably by tests)
-spec query(Tracee :: pid()) -> pid() | undefined.
query(Tracee) when is_pid(Tracee) ->
  return_alloc(Tracee).

% TODO: Is this needed? (probably by tests)
-spec get_backlog() -> Backlog :: backlog().
get_backlog() ->
  gen_server:call(?MODULE, get_backlog, infinity).


%%% ----------------------------------------------------------------------------
%%% Callbacks.
%%% ----------------------------------------------------------------------------

-spec init(Args :: [File :: string()]) -> no_return().
init([File]) ->

  % Create fresh allocations table.
  create_alloc_table(),

  % Start log file poller process.
  log_poller:start_link(File, []),
  {ok, []}.

-spec terminate(Reason, Backlog) -> ok when
  Reason :: normal | shutdown | {shutdown, term()} | term(),
  Backlog :: backlog().
terminate(Reason, _) ->
  Stop = log_poller:stop(),
  delete_alloc_table(),
  ?DEBUG("Tracer stopped for the following reasons ~p and ~p.", [Reason, Stop]).

-spec handle_call(Request, From, Backlog) ->
  {reply, Backlog, Backlog} |
  {reply, ok, NewBacklog :: backlog()} |
  {reply, {unblocked, Ref :: reference()}, Backlog}
  when
  Request :: get_backlog |
  {post, Entry :: log_eval:entry()} |
  {unblock, Ref :: reference()},
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
      % processing must be continually repeated until all events that are meant
      % to be dispatched have been dispatched.
      NewBacklog = replay_backlog(Backlog),
      {reply, ok, NewBacklog}
  end;
handle_call({unblock, Ref}, _From, Backlog) ->
  {reply, {unblocked, Ref}, Backlog}.

-spec handle_cast(Request, Backlog) -> {noreply, UpdatedBacklog :: backlog()}
  when
  Request :: update,
  Backlog :: backlog().
handle_cast(update, Backlog) ->

  % A new tracer allocation has been created externally via the API. This may
  % make events queued in the backlog eligible to being dispatched, and the
  % backlog needs to be processed. Since this processing may itself create
  % additional allocations, the processing must be continually repeated until
  % all events that are meant to be dispatched have been dispatched.
  {noreply, replay_backlog(Backlog)}.


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% @private Dispatches events in the backlog repeatedly until all events that
%% were meant to be dispatched have been dispatched.
%%
%% {@params
%%   {@name Backlog}
%%   {@desc The trace event backlog to process.}
%% }
%% {@par Events in the backlog are processed one by one and guaranteed to be
%%       dispatched in the same order in which these were posted to the tracer.
%% }
%% {@par When the processing of an event results in a new allocation being
%%       automatically made for a <tt><b>fork</b></tt> event (see
%%       {@link play_backlog/1}), processing stops and recommences from the
%%       beginning of the backlog: his ensures that any queued for the process
%%       that have become eligible to being dispatched are dispatched
%%       <i>before</i> any other events for the process in question. The
%%       reprocessing is again repeated should the next iteration again result
%%       in a new allocation, and so on until the progressive application of the
%%       function on intermediate refinements of the backlog yields one that is
%%       free from events pending dispatch (see {@link ref_log_tracer}).
%% }
%% {@returns the fully-processed backlog.}
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

%% @doc Dispatches events in the backlog to tracers whenever a corresponding
%% allocation exists.
%%
%% {@params
%%   {@name Backlog}
%%   {@desc The trace event backlog to process.}
%% }
%% {@par Attempting to dispatch trace events from the backlog leads to three
%%       different outcomes, each of which must be handled accordingly:
%% }
%% {@ol
%%   {@item The trace event under consideration could not be dispatched because
%%          no tracer is allocated at the time. The event is left in the
%%          backlog.
%%   }
%%   {@item The trace event under consideration was dispatched because an
%%          allocated tracer was found. The event is removed from the backlog.
%%   }
%%   {@item The trace event under consideration was dispatched because an
%%          allocated tracer was found. The event is removed from the backlog.
%%          However, the processing of the event in question resulted in a new
%%          allocation being automatically made: the only instance when this
%%          happens is in the case of `fork(P, Q)' events, where the same tracer
%%          `T' for process `P' is also allocated to the forked process `Q'. The
%%          rest of the backlog remains unprocessed.
%%   }
%% }
%%
%% {@returns a result corresponding to one of the above outcomes:
%%   {@dl
%%     {@term `@{stable, ProcessedBacklog@}'}
%%     {@desc The backlog has been processed and all events that were meant to
%%            be dispatched (if any) have been dispatched. This is the case for
%%            outcomes 1 and 2 above.
%%     }
%%     {@term `@{replay, PartiallyProcessedBacklog@}'}
%%     {@desc The backlog processing has been interrupted on account of a new
%%            allocation being automatically created. As this could potentially
%%            make certain queued events in the backlog to be eligible to being
%%            dispatched, ensuing events in the backlog cannot be processed
%%            until all other previous events have been reexamined to determine
%%            whether these can be dispatched. The caller must handle this case
%%            by invoking this function anew with the fresh
%%            `PartiallyProcessedBacklog'. See {@link replay_backlog}.
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

      % Process event was not dispatched to tracer. Skip process and process
      % next.
      {Status, Rest} = play_backlog(Backlog),
      {Status, [Entry | Rest]};

    {true, stable} ->

      % Process event was dispatched to tracer and no update has been made to
      % allocated tracers table. Remove event from backlog and process next.
      play_backlog(Backlog);

    {true, update} ->

      % Process event was dispatched to tracer and an update has been made to
      % allocated tracers table. As this update may affect subsequent event
      % dispatching for the process in question, stop processing remaining
      % backlog events.
      {replay, Backlog}
  end.

%% @doc Creates the allocated tracers table.
%%
%% {@returns the table name.}
-spec create_alloc_table() -> ?ETS_ALLOC_NAME.
create_alloc_table() ->
  % The default locking is on table-level, allowing only one update
  % operation at a time per table.
  % Table option write_concurrency will enable locking on a more fine
  % grained level, allowing concurrent update operations. In current
  % implementation 16 locks per table is used, which result in a probability
  % of 1/16 that two random keys will collide on the same lock.
  % See: http://erlang.org/pipermail/erlang-questions/2012-May/066632.html
  % 'write_concurrency' enables concurrent writes.
  % 'read_concurrency' speeds up concurrent reads.
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
%%   {@desc The PID of the tracee.}
%%   {@name Tracer}
%%   {@desc The PID of the tracer to allocate.}
%% }
%% {@par The operation fails if `Tracee' already exists in the table.}
%%
%% {@returns `true' if the creation succeeded, otherwise `false'.}
-spec create_alloc(Tracee :: pid(), Tracer :: pid()) -> Success :: boolean().
create_alloc(Tracee, Tracer) ->
%%  ?INFO("==== Table BEFORE: ~p~n", [ets:tab2list(?ETS_NAME)]),
  Ret = ets:insert_new(?ETS_ALLOC_NAME, #alloc{tracee = Tracee, tracer = Tracer}),

%%  ?INFO("==== Table AFTER: ~p~n", [ets:tab2list(?ETS_NAME)]),
  Ret.

%% @doc Returns the PID of the allocated tracer for the specified tracee PID.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc The PID of the tracee whose tracer is to be returned.}
%% }
%%
%% {@returns the PID of the tracer if existent, otherwise `undefined'.}
-spec return_alloc(Tracee :: pid()) -> Tracer :: pid() | undefined.
return_alloc(Tracee) ->
  case ets:lookup(?ETS_ALLOC_NAME, Tracee) of
    [] ->
      undefined;
    [#alloc{tracer = Tracer}] ->
      Tracer
  end.

%% @doc Updates the tracer of the associated specified tracee PID.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc The PID of the tracee whose tracer is to be updated.}
%%   {@name Tracer}
%%   {@desc The new tracer PID.}
%% }
%%
%% {@returns `true' if the update succeeded, otherwise `false'.}
-spec update_alloc(Tracee :: pid(), Tracer :: pid()) -> Success :: boolean().
update_alloc(Tracee, Tracer) ->
  ets:update_element(?ETS_ALLOC_NAME, Tracee, {#alloc.tracer, Tracer}).

%% @doc Deletes the entry identified by the specified tracee from the allocated
%% tracers table.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc The PID of the tracee whose entry is to be deleted.}
%% }
%%
%% {@returns `true' regardless of whether the tracee exists.}
-spec delete_alloc(Tracee :: pid()) -> true.
delete_alloc(Tracee) ->
  ets:delete(?ETS_ALLOC_NAME, Tracee).

%% @doc Dispatches the event enclosed in the specified specification.
%%
%% {@params
%%   {@name Spec}
%%   {@desc The specification enclosing the event to be dispatched.}
%% }
%% {@par Trace events are handled according to their type:}
%% {@dl
%%   {@term `fork(P, Q)'}
%%   {@desc If process `P' is allocated a tracer `T', the event is dispatched to
%%          `T'. In addition, `Q' is also allocated `T' so that subsequent
%%           events are directed to it, emulating automatic tracer inheritance
%%           of forked processes from their parents. Nothing happens otherwise.
%%   }
%%   {@term `exit(P)'}
%%   {@desc If process `P' is allocated a tracer `T', the event is dispatched to
%%          `T'. In addition, the allocation of `T' for `P' is removed,
%%          emulating automatic tracer deallocation for terminated processes.
%%          Nothing happens otherwise.
%%   }
%%   {@term `send(P, Q)'}
%%   {@desc If process `P' is allocated a tracer `T', the event is dispatched to
%%          `T'. Nothing happens otherwise.
%%   }
%%   {@term `recv(Q)'}
%%   {@desc If process `Q' is allocated a tracer `T', the event is dispatched to
%%          `T'. Nothing happens otherwise.
%%   }
%% }
%%
%% {@returns `@{true, stable@}' when the event has been dispatched,
%%           `@{true, update@}' when the dispatched and this resulted in an
%%            automatic tracer allocation, and `false' otherwise.
%% }
-spec dispatch(Entry :: log_eval:entry()) ->
  {true, stable} | {true, update} | false.
dispatch(Entry = {delay, _, _Event = {fork, _, Pid2, _}}) ->
  gen_dispatch(Entry,
    fun(Tracer) ->

      ?TRACE("AUTO allocating tracer ~p to forked process ~p (~p).", [Tracer, Pid2, _Event]),

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
dispatch(Entry = {delay, _, _Event = {exit, Pid, _}}) ->
  gen_dispatch(Entry,
    fun(Tracer) ->

      ?TRACE("Deallocating tracer ~p for exiting process ~p (~p).", [Tracer, Pid, _Event]),

      % Automatically deallocate tracer for terminated PID. As this point, it is
      % possible that immediately before this deallocation is effected, an
      % external entity deallocates the tracer via `clear/1'. This race
      % condition does not impinge on event dispatching.
      delete_alloc(Pid),
      stable
    end);
dispatch(Entry = {delay, _, _}) ->
  gen_dispatch(Entry, fun(_) -> stable end).

%% @doc Attempts to dispatch the event enclosed in the specified specification,
%% invoking the callback when successful.
%%
%% {@params
%%   {@name Spec}
%%   {@desc The specification enclosing the event to be dispatched.}
%%   {@name OnDispatch}
%%   {@desc The callback function accepting a single parameter `Tracer' that
%%          corresponds to the allocated tracer PID.
%%   }
%% }
%% {@par The event is dispatched <i>only</i> when a tracer allocation is found.}
%%
%% {@returns `@{true, Result@}' when the event has been dispatched, otherwise
%%           `false'. `Result' is the return result of the callback function.
%% }
-spec gen_dispatch(Entry, OnDispatch) -> Status :: {true, any()} | false
  when
  Entry :: log_eval:entry(),
  OnDispatch :: function().
gen_dispatch({delay, _Ms, Event}, OnDispatch) when is_function(OnDispatch, 1) ->
  Pid = element(2, Event),
  case return_alloc(Pid) of
    undefined ->

      ?TRACE("No tracer allocated to ~p: placing ~p on backlog.", [Pid, Event]),

      % No tracer allocated to PID - withhold dispatching.
      false;
    Tracer when is_pid(Tracer) ->

      % Invoke callback for optional pre-processing.
      Return = OnDispatch(Tracer),

      % A tracer is allocated to source PID.
      ?TRACE("Tracer ~p allocated to ~p: dispatching ~p.", [Tracer, Pid, Event]),

      % Implementation note: We could have instead modified the evm_tracer
      % module and added a translator that acts as a proxy, rewriting
      % EVM-specific trace events into a more general form (e.g., using fork,
      % exit, send, receive). This would however require introducing an extra
      % translator process for each tracer (introducing just a common one would
      % not do because it would create a bottle neck and introduce a possible
      % single point of failure). Since in our experiments we'll most likely be
      % using EVM tracing, we want to keep the latter as efficient as possible
      % eliminating altogether the translation. Instead, we opt to do this
      % translation for offline mode, were timing constraints are somewhat less
      % pressing. This is a small price to pay to keep the message format of the
      % EVM (but it's not that bad). For other implementations (e.g., DB or RSS)
      % we would have to do something similar.
      Tracer ! event:to_evm_event(Event),

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
