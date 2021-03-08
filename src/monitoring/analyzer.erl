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
-module(analyzer).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([start/4, stop/1]).
-export([dispatch/1, do_monitor/2, filter/1]).

%%% Callbacks/Internal.
-export([init/3]).

%%% Types.
-export_type([]).

%%% Implemented behaviors.
%-behavior().


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Process dictionary key used to store the synthesised analysis function that
%% is applied to trace events. The result of this function application is used
%% to overwrite the previous result.
-define(MONITOR, '$monitor').

%% Three types of irrevocable verdicts reached by the analysis.
-define(VERDICT_YES, yes).
-define(VERDICT_NO, no).
-define(VERDICT_END, 'end').


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type verdict() :: ?VERDICT_YES | ?VERDICT_NO | ?VERDICT_END.
%% Three analysis verdicts.

%% TODO: IMO, should also add monitor() to the below, monitor() | verdict(). Check with dialyzer.
-type monitor() :: fun((Event :: term()) -> verdict() | no_return()).
%% Analyzer that accepts a trace event and transitions to its subsequent
%% unfolded continuation, or a verdict stage when the no such transitions are
%% possible. An analyzer can also diverge indefinitely, in which case events are
%% consumed albeit a final verdict is never reached.

-type mfa_spec() :: fun((Mfa :: mfa()) -> {ok, monitor()} | undefined).
%% Function mapping that specified the forked function calls that are to be
%% monitored. For the case of erlang, these function calls are always external
%% calls, such as those issued via {@link spawn/3}.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Starts the analyzer.
%%
%% {@params
%%   {@name Tracer}
%%   {@desc PID of tracer process starting the analyzer.}
%%   {@name Owner}
%%   {@desc }
%%   {@name }
%%   {@desc }
%%   {@name }
%%   {@desc }
%% }
%%
%% {@returns PID of the analyzer process.}
-spec start(Tracer, Owner, Pid, Monitor) -> Pid :: pid()
  when
  Tracer :: pid(),
  Owner :: tracer:owner(),
  Pid :: pid(),
  Monitor :: monitor().
start(Tracer, Owner, Pid, Monitor) ->
  spawn(fun() -> put(?MONITOR, Monitor), init(Tracer, Owner, Pid) end).

%% @doc Stops the asynchronous monitor identified by the specified Pid.
-spec stop(Pid :: pid()) -> Ref :: reference().
stop(Pid) ->
  util:rpc_async(Pid, stop).

%% @private Monitor initialization.
-spec init(Tracer, Owner, Pid) -> no_return()
  when
  Tracer :: pid(),
  Owner :: tracer:owner(),
  Pid :: pid().
init(Tracer, Owner, Pid) ->
  if is_pid(Owner) -> link(Owner); true -> ok end,
  loop(Tracer, Owner, Pid).

%% @private Main monitor loop.
-spec loop(Tracer, Owner, PidS) -> no_return()
  when
  Tracer :: pid(),
  Owner :: tracer:owner(),
  PidS :: pid().
loop(Tracer, Owner, PidS) ->
  receive
    {From, _, stop} ->

      % There should be no more trace messages left when the stop command is
      % received.
      ?assertEqual({messages, []}, process_info(self(), messages)),

      ?INFO("Monitor received STOP command from tracer ~w.", [From]),
      exit({garbage_collect, {monitor, ?VERDICT_END}});

    Event ->

      % At this point, the monitor should only receive trace events. Events
      % should also be of specific types.
      ?assertEqual(trace, element(1, Event)),
      ?assert(
        element(3, Event) =:= spawn orelse element(3, Event) =:= exit orelse
          element(3, Event) =:= send orelse element(3, Event) =:= 'receive' orelse
          element(3, Event) =:= spawned
      ),

      % Analyze event and garbage collect monitor is verdict is reached.
      do_monitor(Event,
        fun(Verdict) -> exit({garbage_collect, {monitor, Verdict}}) end
      ),
      % TODO: Test this

      loop(Tracer, Owner, PidS)
  end.


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------


%% @doc Dispatches the specified abstract event to the monitor for analysis.
%%
%% {@params
%%   {@name Event}
%%   {@desc The abstract event that the monitor is to analyze.}
%% }
%%
%% {@returns Depends on the event type. See {@link trace_lib:event/0}.
%%           {@ul
%%             {@item When event is of type `fork', the PID of the new child
%%                    process is returned;
%%             }
%%             {@item When event is of type `init', the PID of the parent
%%                    process is returned;
%%             }
%%             {@item When event is of type `exit', the exit reason is returned;}
%%             {@item When event is of type `send', the message is returned;}
%%             {@item When event is of type `recv', the message is returned.}
%%           }
%% }
-spec dispatch(Event :: events:event()) -> term().
dispatch(Event = {fork, _Parent, Child, _Mfa}) ->
  do_monitor(events:to_evm_event(Event),
    fun(Verdict) -> ?INFO("Reached verdict '~s' after ~w.", [Verdict, Event]) end
  ),
  Child;
dispatch(Event = {init, _Child, Parent, _Mfa}) ->
  do_monitor(events:to_evm_event(Event),
    fun(Verdict) -> ?INFO("Reached verdict '~s' after ~w.", [Verdict, Event]) end
  ),
  Parent;
dispatch(Event = {exit, _Process, Reason}) ->
  do_monitor(events:to_evm_event(Event),
    fun(Verdict) -> ?INFO("Reached verdict '~s' after ~w.", [Verdict, Event]) end
  ),
  Reason;
dispatch(Event = {send, _Sender, _Receiver, Msg}) ->
  do_monitor(events:to_evm_event(Event),
    fun(Verdict) -> ?INFO("Reached verdict '~s' after ~w.", [Verdict, Event]) end
  ),
  Msg;
dispatch(Event = {recv, _Receiver, Msg}) ->
  do_monitor(events:to_evm_event(Event),
    fun(Verdict) -> ?INFO("Reached verdict '~s' after ~w.", [Verdict, Event]) end
  ),
  Msg.


%% @doc Retrieves the monitor function stored in the process dictionary (if
%% any), and applies it on the event. The result is put back in the process
%% dictionary. If a verdict state is reached, the callback function is invoked,
%% otherwise nothing is done. When no monitor function is stored inside the
%% process dictionary (i.e. meaning that the process is not monitored), the atom
%% `undefined' is returned.
-spec do_monitor(Event, VerdictFun) -> Monitor :: monitor() | undefined
  when
  Event :: trace_lib:evm_event(),
  VerdictFun :: fun((Verdict :: verdict()) -> any()).
do_monitor(Event, VerdictFun) when is_function(VerdictFun, 1) ->
  case get(?MONITOR) of
    undefined ->
      ?TRACE("Monitor not defined; not handling trace event ~w.", [Event]),
      undefined;
    Monitor ->

      % Analyze event. At this point, monitor might have reached a verdict.
      % Check whether verdict is reached to enable immediate detection, should
      % this be the case.
      put(?MONITOR, Monitor0 = analyze(Monitor, Event)),
      case is_verdict(Monitor0) of
        true ->
          VerdictFun(Monitor0);
        false ->
          ok
      end,
      Monitor0
  end.

% TODO: Include another submit function that is used to submit asynchronously
% TODO: to the monitor. This may be used by the tracer module, instead of
% TODO: directly using ! to send messages to monitor. Problem is that the events
% TODO: submitted by the tracer are EVM events, whereas the submit above uses
% TODO: abstract events :(

%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% @private Determines whether the specified monitor is indeed a verdict.
-spec is_verdict(Verdict :: term()) -> boolean().
is_verdict(Verdict) when Verdict =:= yes; Verdict =:= no; Verdict =:= 'end' ->
  true;
is_verdict(_) ->
  false.

%% @private Effects the analysis by applying the monitor function to the
%% specified event. If a verdict state is reached, the event is silently
%% discarded.
-spec analyze(Monitor, Event) -> Monitor0 :: monitor()
  when
  Monitor :: monitor(),
  Event :: trace_lib:event().
analyze(Monitor, Event) ->
  case is_verdict(Monitor) of
    true ->

      % Monitor is at the verdict state, and the event, even though it is
      % analyzed, does not alter the current verdict.
      ?TRACE("Monitor analyzing event ~w and reached verdict '~s'.",
        [Event, Monitor]),
      Monitor;
    false ->

      % Monitor is not yet at the verdict state and can analyze the event.
      % Return next monitor unfolding.
      ?TRACE("Monitor analyzing event ~w.", [Event]),
      Monitor(Event)
  end.

%% @doc Default filter that allows all events to pass.
-spec filter(Event :: events:event()) -> true.
filter(_) ->
  true. % True = keep event.