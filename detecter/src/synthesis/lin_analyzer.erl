%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Module description (becomes module heading).
%%%
%%% @end
%%% 
%%% Copyright (c) 2022, Duncan Paul Attard <duncanatt@gmail.com>
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
-module(lin_analyzer).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%-compile(export_all).

%%% Public API.
-export([reduce_tau/2, analyze/3]).
-export([analyze_trace/2, analyze_file/2, show_pdlist/1]).
-export([embed/1, dispatch/1]).

%%% Callbacks/Internal.
-export([]).

%%% Types.
-export_type([rule/0, verdict/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Process dictionary key used to store the synthesized analysis function that
%% is applied to trace events. The result of this function application is used
%% to overwrite the previous result.
-define(MONITOR, '$monitor').

%% Irrevocable verdicts reached by the runtime analysis.
-define(VERDICT_YES, yes).
-define(VERDICT_NO, no).

%% Small-step semantics rule name identifiers; these rules dictate how a monitor
%% is reduced from one state to the next.
-define(M_VRD, mVrd). % Verdict persistence.
-define(M_ACT, mAct). % Action analysis.
-define(M_CHS_L, mChsL). % Left external choice.
-define(M_CHS_R, mChsR). % Right external choice.
-define(M_TAU_L, mTauL). % Left tau reduction.
-define(M_TAU_R, mTauR). % Right tau reduction.
-define(M_PAR, mPar). % Lockstep action reduction.
-define(M_DIS_Y_L, mDisYL). % Yes verdict left disjunction short circuiting.
-define(M_DIS_Y_R, mDisYR). % Yes verdict right disjunction short circuiting.
-define(M_DIS_N_L, mDisNL). % No verdict left disjunction short circuiting.
-define(M_DIS_N_R, mDisNR). % No verdict right disjunction short circuiting.
-define(M_CON_Y_L, mConYL). % Yes verdict left conjunction short circuiting.
-define(M_CON_Y_R, mConYR). % Yes verdict right conjunction short circuiting.
-define(M_CON_N_L, mConNL). % No verdict left conjunction short circuiting.
-define(M_CON_N_R, mConNR). % No verdict right conjunction short circuiting.
-define(M_REC, mRec). % Monitor unfolding.

%% Monitor environment keys.
-define(KEY_ENV, env).
-define(KEY_STR, str).
-define(KEY_VAR, var).
-define(KEY_PAT, pat).
-define(KEY_CTX, ctx).
-define(KEY_NS, ns).

%% Monitor and proof display macros.
-define(PD_SEP, "-").
-define(INDENT(PdId), (length(PdId) + length(?PD_SEP))).

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type rule() :: ?M_TAU_L | ?M_TAU_R | ?M_REC |
?M_DIS_Y_L | ?M_DIS_Y_L | ?M_DIS_N_L | ?M_DIS_N_R |
?M_CON_Y_L | ?M_CON_Y_R | ?M_CON_N_L | ?M_CON_N_R |
?M_VRD | ?M_ACT | ?M_CHS_L | ?M_CHS_R | ?M_PAR.
%% Small-step semantics rule names.

-type verdict() :: ?VERDICT_NO | ?VERDICT_YES.
%% Verdicts reachable by the runtime analysis.

-type monitor() :: term().
%% Monitor type.

-type pd_id() :: list(integer()).
%% Proof derivation ID.

-type tau() :: tau.
%% Monitor internal silent transition.

-type event() :: any().
%% Analysable trace event actions.

-type premise() :: {pre, pd()}.
%% Proof derivation premise.

-type pd() :: {pd_id(), rule(), event(), monitor(), monitor()} |
{pd_id(), rule(), event(), monitor(), monitor(), premise()} |
{pd_id(), rule(), event(), monitor(), monitor(), monitor(), premise(), premise()}.
%% Three types of proof derivations. One is for axioms that have no premises,
%% one for derivations having one premise, and one for derivations having two
%% premises.

-type env() :: {?KEY_ENV, list()}.
-type str() :: {?KEY_STR, string()}.
-type var() :: {?KEY_VAR, atom()}.
-type pat() :: {?KEY_PAT, term()}.
-type ctx() :: {?KEY_CTX, list()}.
-type ns() :: {?KEY_NS, atom()}.
-type binding() :: {{atom(), atom()}, any()}.


%%% ----------------------------------------------------------------------------
%%% Public event analysis API.
%%% ----------------------------------------------------------------------------

%% @doc Reduces the specified monitor using tau transitions repeatedly until it
%%      cannot be reduced further.
%%
%% {@params
%%   {@name M}
%%   {@desc Monitor to reduce.}
%%   {@name PdList}
%%   {@desc Initial list of proof derivations. New derivations are added to the
%%          head of the list.
%%   }
%% }
%%
%% {@par The monitor is left in a state that is ready to analyze trace events.}
%%
%% {@returns Proof derivation list and reduced monitor. If the specified monitor
%%           cannot be reduced, the original monitor is returned, and the proof
%%           derivation list is empty.
%% }
-spec reduce_tau(M :: monitor(), PdList :: list(pd())) -> {list(pd()), monitor()}.
reduce_tau(M, PdList) ->
  ?TRACE("[ Attempting new derivation for monitor on internal action 'tau' ]"),

  case derive_tau(M, new_pdid([])) of
    false ->

      % No more tau reductions.
      {PdList, M};
    {true, {PdM, M_}} ->

      % Monitor state reduced by one tau transition. Attempt to reduce further.
      reduce_tau(M_, [PdM | PdList])
  end.

%% @doc Analyzes the specified trace event and performs the monitor reduction
%%      accordingly.
%%
%% {@params
%%   {@name Event}
%%   {@desc Trace event to analyze.}
%%   {@name M}
%%   {@desc Monitor to analyze the trace event.}
%%   {@name PdList}
%%   {@desc Initial list of proof derivations. New derivations are added to the
%%          head of the list.}
%% }
%% {@par The reductions effected are added to the head of the specified proof
%%       derivation list. The function assumes that the monitor supplied is
%%       already in the ready state. See {@link lin_analyzer:reduce_tau/2}.
%% }
%%
%% {@returns Proof derivation list and reduced monitor. The specified monitor
%%           is guaranteed to be reduced by at one or more steps, where the
%%           first analyzes the specified trace event.
%% }
-spec analyze(Event, M, PdList) -> {list(pd()), monitor()}
  when
  Event :: event:evm_event(),
  M :: monitor(),
  PdList :: list(pd()).
analyze(Event, M, PdList) ->
  ?TRACE("[ Starting new derivation for monitor on event '~w' ]", [Event]),

  % Analyze trace event.
  {PdM, M_} = derive_act(Event, M, new_pdid([])),

  % Check whether the residual monitor state can be reduced further using tau
  % transitions. This ensures that the monitor is always left in a state where
  % it is ready to analyse the next action.
  reduce_tau(M_, [PdM | PdList]).

%% @doc Analyzes the list of events as a trace using the specified monitor.
%%
%% {@params
%%   {@name Trace}
%%   {@desc Trace of events to analyze.}
%%   {@name M}
%%   {@desc Monitor to analyze the trace event list.}
%% }
%%
%% {@par The specified monitor is automatically reduced to the ready state prior
%%       the commencement of trace event analysis.
%% }
%%
%% {@returns Proof derivation list and reduced monitor. The specified monitor
%%           is guaranteed to be reduced by at one or more steps, where the
%%           first analyzes the specified trace event.
%% }
-spec analyze_trace(Trace, M) -> {list(pd()), monitor()}
  when
  Trace :: list(event:int_event()),
  M :: monitor().
analyze_trace(Trace, M) when is_list(Trace) ->
  {PdList_, M_} = reduce_tau(M, []),
  analyze_trace(Trace, M_, PdList_).

%% @doc Analyzes the list of events as a trace using the specified monitor.
%%
%% {@params
%%   {@name File}
%%   {@desc File where the saved trace log is located.}
%%   {@name M}
%%   {@desc Monitor to analyze the trace event file.}
%% }
%%
%% {@par The specified monitor is automatically reduced to the ready state prior
%%       the commencement of trace event analysis.
%% }
%%
%% {@returns Proof derivation list and reduced monitor. The specified monitor
%%           is guaranteed to be reduced by at one or more steps, where the
%%           first analyzes the specified trace event.
%% }
-spec analyze_file(File, M) -> {list(pd()), monitor()}
  when
  File :: string(),
  M :: monitor().
analyze_file(File, M) when is_list(File) ->

  % Load trace from file as a list of Erlang terms and analyze.
  {ok, Trace} = file:consult(File),
  analyze_trace(Trace, M).

%% @doc Formats and displays the specified proof derivation list to the standard
%%      output.
%%
%% {@params
%%   {@name PdList}
%%   {@desc Proof derivation list to format.}
%% }
%%
%% {@returns ok.}
-spec show_pdlist(PdList :: list(pd())) -> ok.
show_pdlist(PdList) ->
  {_, IoList} = format_pdlist(PdList),
  io:format("~s~n", [IoList]).


%%% ----------------------------------------------------------------------------
%%% Public monitor instrumentation and weaving support API.
%%% ----------------------------------------------------------------------------

%% @doc Embeds the trace event analysis function into the process dictionary.
%%
%% {@params
%%   {@name M}
%%   {@desc Monitor function that is applied to trace events to determine their
%%          correct or incorrect sequence.
%%   }
%% }
%%
%% {@returns `true' to indicate success, otherwise `false'.}
-spec embed(M :: monitor()) -> true.
embed(M) ->
  ?TRACE("Embedding monitor in ~w.", [self()]),

  % Reduce monitor internally until it is in a state where it can analyze the
  % next trace event.
  {PdList_, M_} = reduce_tau(M, []),
  undefined =:= put(?MONITOR, {PdList_, M_}).

%% @doc Dispatches the specified abstract event to the monitor for analysis.
%%
%% {@params
%%   {@name Event}
%%   {@desc The abstract event that the monitor is to analyze.}
%% }
%%
%% {@returns Depends on the event type. See {@link event:event/0}.
%%           {@ul
%%             {@item When event is of type `fork', the PID of the new child
%%                    process is returned;
%%             }
%%             {@item When event is of type `init', the PID of the parent
%%                    process is returned;
%%             }
%%             {@item When event is of type `exit', the exit reason is
%%                    returned;
%%             }
%%             {@item When event is of type `send', the message is returned;}
%%             {@item When event is of type `recv', the message is returned.}
%%           }
%% }
-spec dispatch(Event :: event:int_event()) -> term().
dispatch(Event = {fork, _Parent, Child, _Mfa}) ->
  do_monitor(event:to_evm_event(Event),
    fun(Verdict, _) ->
      format_verdict("After analyzing event ~w.~n", [Event], Verdict)
    end
  ),
  Child;
dispatch(Event = {init, _Child, Parent, _Mfa}) ->
  do_monitor(event:to_evm_event(Event),
    fun(Verdict, _) ->
      format_verdict("After analyzing event ~w.~n", [Event], Verdict)
    end
  ),
  Parent;
dispatch(Event = {exit, _Process, Reason}) ->
  do_monitor(event:to_evm_event(Event),
    fun(Verdict, _) ->
      format_verdict("After analyzing event ~w.~n", [Event], Verdict)
    end
  ),
  Reason;
dispatch(Event = {send, _Sender, _Receiver, Msg}) ->
  do_monitor(event:to_evm_event(Event),
    fun(Verdict, _) ->
      format_verdict("After analyzing event ~w.~n", [Event], Verdict)
    end
  ),
  Msg;
dispatch(Event = {recv, _Receiver, Msg}) ->
  do_monitor(event:to_evm_event(Event),
    fun(Verdict, _) ->
      format_verdict("After analyzing event ~w.~n", [Event], Verdict)
    end
  ),
  Msg.


%%% ----------------------------------------------------------------------------
%%% Private derivation functions.
%%% ----------------------------------------------------------------------------

%%% @doc Determines the rule that must be applied to reduce the monitor state by
%%%      one tau transition.
%%%
%%% {@params
%%%   {@name M}
%%%   {@desc Monitor to be reduced.}
%%%   {@name PdId}
%%%   {@desc Proof derivation ID.}
%%% }
%%%
%%% {@returns `true' together with the proof derivation and monitor continuation
%%%           after one tau reduction or `false' if the monitor cannot be
%%%           reduced.
%%% }
-spec derive_tau(M, PdId) -> false | {true, {pd(), monitor()}}
  when
  M :: monitor(),
  PdId :: pd_id().
derive_tau(L = {'or', _, Yes = {yes, _}, _}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mDisYL: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),

  % Axiom mDisYL.
  {true, {{PdId, ?M_DIS_Y_L, tau, L, Yes}, Yes}};

derive_tau(L = {'or', _, _, Yes = {yes, _}}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mDisYR: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),

  % Axiom mDisYR.
  {true, {{PdId, ?M_DIS_Y_R, tau, L, Yes}, Yes}};

derive_tau(L = {'or', _, {no, _}, M}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mDisNL: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),

  % Axiom mDisNL.
  {true, {{PdId, ?M_DIS_N_L, tau, L, copy_ns(L, copy_ctx(L, M))}, copy_ns(L, copy_ctx(L, M))}};

derive_tau(L = {'or', _, M, {no, _}}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mDisNR: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),

  % Axiom mDisNR.
  {true, {{PdId, ?M_DIS_N_R, tau, L, copy_ns(L, copy_ctx(L, M))}, copy_ns(L, copy_ctx(L, M))}};

derive_tau(L = {'and', _, {yes, _}, M}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mConYL: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),

  % Axiom mConYL.
  {true, {{PdId, ?M_CON_Y_L, tau, L, copy_ns(L, copy_ctx(L, M))}, copy_ns(L, copy_ctx(L, M))}};

derive_tau(L = {'and', _, M, {yes, _}}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mConYR: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),

  % Axiom mConYR.
  {true, {{PdId, ?M_CON_Y_R, tau, L, copy_ns(L, copy_ctx(L, M))}, copy_ns(L, copy_ctx(L, M))}};

derive_tau(L = {'and', _, No = {no, _}, _}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mConNL: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),

  % Axiom mConNL.
  {true, {{PdId, ?M_CON_N_L, tau, L, No}, No}};

derive_tau(L = {'and', _, _, No = {no, _}}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mConNR: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),

  % Axiom mConNR.
  {true, {{PdId, ?M_CON_N_R, tau, L, No}, No}};

derive_tau(L = {rec, Env, M}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mRec: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),

  % The continuation of a recursion construct is encoded as a function that
  % needs to be applied to unfold the monitor. Recursion monitors do not accept
  % parameters. Axiom mRec.
  M_ = M(),

  % Create a new namespace that enables the monitor to track the recursion
  % context that the data binder is in. This is used so that when a recursive
  % variable is unfolded, all the variables in that context are cleared. This
  % garbage collection of variables can be done since an unfolded monitor with
  % data variables shadows the variables in the previous unfolding. The clearing
  % of data variables is handled by the clause underneath.
  M__ = set_env(M_, set_ns(get_env(M_), {ns, unwrap_value(get_var(Env))})),

  {true, {{PdId, ?M_REC, tau, L, copy_ctx(L, M__)}, copy_ctx(L, M__)}};

derive_tau(L = {var, Env, M}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mRec (var): ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),

  % Recursive variables complement recursive constructs, and are used to refer
  % to recursive monitor definitions. Identically to the recursive construct,
  % the variable itself is a function reference that needs to be applied to
  % unfold the monitor. Recursive monitor definitions do not accept parameters.

  % Recursion variables complement recursion definition constructs. Recursion
  % variables invoke recursion monitor definitions. The recursion variable
  % refers to the same function defined by the recursion definition construct,
  % and needs to be applied to unfold the monitor. Recursion monitor definitions
  % do not accept parameters. Axiom mRec.
  M_ = M(),

  % Purge all the variables in the binding context that are in the current
  % recursion context.
  Ctx = clean_ns(get_ctx(Env), unwrap_value(get_ns(Env))),
  L_ = set_env(L, set_ctx(Env, Ctx)),

  {true, {{PdId, ?M_REC, tau, L, copy_ctx(L_, M_)}, copy_ctx(L_, M_)}};

derive_tau(L = {Op, Env, M, N}, PdId) when Op =:= 'and'; Op =:= 'or' ->

  % Parallel monitors may transition internally by either reducing their left or
  % right sub-monitor constituents. The formal semantics presented in the paper
  % leave this selection unspecified. This implementation opts for trying to
  % reduce the left sub-monitor first, then the right one, if, and only if the
  % former reduction is not possible.
  ?DEBUG(":: (~s) Trying to reduce using rule mTauL: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),
  case derive_tau(copy_ns(L, copy_ctx(L, M)), new_pdid(PdId)) of
    false ->
      ?DEBUG(":: (~s) Trying to reduce using rule mTauR: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),
      case derive_tau(copy_ns(L, copy_ctx(L, N)), new_pdid(PdId)) of
        false ->
          ?DEBUG(":: (~s) Unable to reduce using mTauL or mTauR: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),
          false;
        {true, {PdN_, N_}} ->

          % Rule mTauR.
          {true, {{PdId, ?M_TAU_R, tau, L, copy_ns(L, copy_ctx(L, M)), N_, {pre, PdN_}}, {Op, Env, M, N_}}}
      end;
    {true, {PdM_, M_}} ->

      % Rule mTauL.
      {true, {{PdId, ?M_TAU_L, tau, L, M_, copy_ns(L, copy_ctx(L, N)), {pre, PdM_}}, {Op, Env, M_, N}}}
  end;

derive_tau(_, _) ->

  % The monitor cannot transition internally on tau actions.
  false.

%%% @doc Determines the rule that must be applied to reduce the monitor state by
%%%      one trace event.
%%%
%%% {@params
%%%   {@name Event}
%%%   {@desc Trace event to analyze.}
%%%   {@name M}
%%%   {@desc Monitor to be reduced.}
%%%   {@name PdId}
%%%   {@desc Proof derivation ID.}
%%% }
%%%
%%% {@returns Proof derivation and monitor continuation after one event
%%%           reduction.
%%% }
-spec derive_act(Event, M, PdId) -> {pd(), monitor()}
  when
  Event :: event(),
  M :: monitor(),
  PdId :: pd_id().
derive_act(Event, M = {V, _}, PdId) when V =:= yes; V =:= no ->
  ?assertNot(Event =:= tau),
  ?DEBUG(":: (~s) Reducing using axiom mVrd: ~s.", [pdid_to_iolist(PdId), m_to_iolist(M)]),

  % Axiom mVrd.
  {{PdId, ?M_VRD, Event, M, M}, M};

derive_act(Event, L = {act, Env, C, M}, PdId) ->
  ?assertNot(Event =:= tau),
%%  ?assert(C(Event)),
  ?assert(is_function(M, 1)),

  % Get the variable binder associated with this action.
  Binder = unwrap_value(get_var(Env)),

  % Instantiate the variable binder with data from the trace event, and extend
  % the variable context. Variables in the context are tagged with the current
  % namespace, which is the scope of the current recursion definition.
  % Populating data variables is used for debugging purposes.
  Ns = get_ns(Env),
  L_ = set_env(L, set_ctx(Env, new_binding(get_ctx(Env), unwrap_value(Ns), Binder, Event))),

  ?DEBUG(":: (~s) Reducing using rule mAct: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L_)]),

  % Monitor actions are encoded as functions. The function needs to be applied
  % to the event being analysed to unfold the monitor. Axiom mAct.
  M_ = M(Event),
  ?assertNot(is_function(M_)),

  % Copy the variable binding context of the current monitor to the binding
  % context of the next unfolding.
  {{PdId, ?M_ACT, Event, L, copy_ns(L_, copy_ctx(L_, M_))}, copy_ns(L_, copy_ctx(L_, M_))}; % Updated monitor env.

derive_act(Event, L = {chs, _, M, N}, PdId) ->
  ?assert(is_tuple(M) andalso element(1, M) =:= act),
  ?assert(is_tuple(N) andalso element(1, N) =:= act),

  % Monitors can transition externally by either reducing their left of right
  % sub-monitor constituents. This reduction is based on whether a sub-monitor
  % is able to transition via a trace event. The synthesis generates monitors
  % where the conditions of the left and right sub-monitors are
  % mutually-exclusive. This enables the analysis to check which branch can be
  % reduced before effecting the sub-monitor reduction.
  case {is_satisfied(Event, M), is_satisfied(Event, N)} of
    {true, false} ->
      ?DEBUG(":: (~s) Reducing using rule mChsL: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),

      % Rule mChsL.
      {PdM_, M_} = derive_act(Event, copy_ns(L, copy_ctx(L, M)), new_pdid(PdId)),
      {{PdId, ?M_CHS_L, Event, L, M_, {pre, PdM_}}, M_};

    {false, true} ->
      ?DEBUG(":: (~s) Reducing using rule mChsR: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),

      % Rule mChsR.
      {PdN_, N_} = derive_act(Event, copy_ns(L, copy_ctx(L, N)), new_pdid(PdId)),
      {{PdId, ?M_CHS_R, Event, L, N_, {pre, PdN_}}, N_}
  end;

derive_act(Event, L = {Op, Env, M, N}, PdId) when Op =:= 'and'; Op =:= 'or' ->
  ?assertNot(Event =:= tau),
  ?DEBUG(":: (~s) Reducing using rule mPar: ~s.", [pdid_to_iolist(PdId), m_to_iolist(L)]),

  % Unfold respective sub-monitors. Proof derivation ID for second monitor N is
  % incremented accordingly.
  {PdM_, M_} = derive_act(Event, copy_ns(L, copy_ctx(L, M)), new_pdid(PdId)),
  {PdN_, N_} = derive_act(Event, copy_ns(L, copy_ctx(L, N)), inc_pdid(new_pdid(PdId))),

  % Merge context of M and N monitors.
  Ctx = merge_ctx(get_ctx(get_env(M_)), get_ctx(get_env(N_))),
  Env_ = set_ctx(Env, Ctx),

  {{PdId, ?M_PAR, Event, L, M_, N_, {pre, PdM_}, {pre, PdN_}}, {Op, Env_, M_, N_}}.

%%% @private Determines whether the monitor constraint is satisfied by the trace
%%%          event.
is_satisfied(Event, {act, _, C, _M}) ->
  ?assert(is_function(_M, 1)),
  C(Event).

%% @private Analyzes trace events individually, updating the monitor state and
%% proof derivation list. Proof derivations are added to the head of the list.
%% To obtain the derivations in the order these were applied, the list needs to
%% be reversed.
-spec analyze_trace(Trace, M, PdList) -> {list(pd()), monitor()}
  when
  Trace :: list(event:int_event()),
  M :: monitor(),
  PdList :: list(pd()).
analyze_trace([], M, PdList) ->
  {PdList, M};
analyze_trace([Event | Trace], M, PdList) ->
%%  {PdList_, M_} = analyze(Event, M, PdList), % TODO: Use this to analyze a simple trace of non-Erlang trace objects.
%%  {PdList_, M_} = analyze(event:to_evm_event(Event), M, PdList),
  {PdList_, M_} = analyze(Event, M, PdList),
  analyze_trace(Trace, M_, PdList_).


%%% ----------------------------------------------------------------------------
%%% Monitor environment management functions.
%%% ----------------------------------------------------------------------------

%%% @private Returns the value mapped to the specified key from the provided
%%%          list. If the key is not found, the default value settles the return
%%%          value: when default is false, false is returned, otherwise the pair
%%%          {key, default value} is returned.
-spec get_key(Key, List, Default) -> false | {Key, any()}
  when
  Key :: any(),
  List :: list(),
  Default :: false | {any(), any()}.
get_key(Key, List, false) ->
  lists:keyfind(Key, 1, List);
get_key(Key, List, {true, Default}) ->
  case get_key(Key, List, false) of
    false ->
      {Key, Default};
    Pair = {Key, _} ->
      Pair
  end.

%%% @private Adds the specified key-value pair to the list.
-spec put_key(Key, Value, List) -> list()
  when
  Key :: any(),
  Value :: any(),
  List :: list().
put_key(Key, Value, List) ->
  lists:keystore(Key, 1, List, {Key, Value}).

%%% @private Returns the environment associated with the current monitor state.
-spec get_env(M :: monitor()) -> env().
get_env(M) when is_tuple(M), tuple_size(M) > 1 ->
  {env, _} = element(2, M).

%%% @private Overwrites the environment of the specified monitor with the new
%%%          one.
-spec set_env(M :: monitor(), Env :: env()) -> monitor().
set_env(M, {env, Env}) when is_tuple(M), tuple_size(M) > 1, is_list(Env) ->
  setelement(2, M, {env, Env}).

%%% @private Returns the string from the specified environment.
-spec get_str(Env :: env()) -> str().
get_str({?KEY_ENV, Env}) when is_list(Env) ->
  get_key(?KEY_STR, Env, false).

%%% @private Returns the variable from the specified environment.
-spec get_var(Env :: env()) -> var().
get_var({?KEY_ENV, Env}) when is_list(Env) ->
  get_key(?KEY_VAR, Env, false).

%%% @private Returns the event pattern from the specified environment.
-spec get_pat(Env :: env()) -> pat().
get_pat({?KEY_ENV, Env}) when is_list(Env) ->
  get_key(?KEY_PAT, Env, false).

%%% @private Returns the existing variable binding context or a fresh one if it
%%%          does not exist.
-spec get_ctx(Env :: env()) -> ctx().
get_ctx({?KEY_ENV, Env}) when is_list(Env) ->
  get_key(?KEY_CTX, Env, {true, []}).

%%% @private Overwrites the variable binding context in the specified monitor
%%%          environment with the new one.
-spec set_ctx(Env :: env(), Ctx :: ctx()) -> env().
set_ctx({?KEY_ENV, Env}, {ctx, Ctx}) when is_list(Env), is_list(Ctx) ->
  {?KEY_ENV, put_key(?KEY_CTX, Ctx, Env)}.

%%% @private Copies the variable binding context from the environment of `From`
%%%          to `To`. Any variable bindings in the target context `To` are
%%%          discarded.
-spec copy_ctx(From :: monitor(), To :: monitor()) -> monitor().
copy_ctx(From, To) ->
  EnvTo = set_ctx(get_env(To), get_ctx(get_env(From))),
  set_env(To, EnvTo).

%%% @private Merges the specified variable binding contexts into a new one. In
%%%          case of duplicate variable names, the bindings in the second
%%%          context `Ctx2` are preferred, and the ones in `Ctx1` are
%%%          overwritten.
-spec merge_ctx(Ctx1 :: ctx(), Ctx2 :: ctx()) -> ctx().
merge_ctx({?KEY_CTX, Ctx1}, {?KEY_CTX, Ctx2}) ->
  {?KEY_CTX, lists:foldr(
    fun(Mapping = {Name, _}, Acc) ->
      case get_key(Name, Acc, false) of
        false ->
          [Mapping | Acc];
        {Name, _} ->
          Acc
      end
    end, Ctx2, Ctx1)}.

%%% @private Creates a new variable binding and value under the specified
%%%          namespace in the specified variable binding context.
-spec new_binding(Ctx, Ns, Name, Value) -> ctx()
  when
  Ctx :: ctx(),
  Ns :: ns(),
  Name :: atom(),
  Value :: any().
new_binding({?KEY_CTX, Ctx}, Ns, Name, Value) when is_list(Ctx) ->
  {?KEY_CTX, put_key({Ns, Name}, Value, Ctx)}.

%%% @private Purges all variable bindings under the specified namespace from the
%%%          specified variable binding context.
-spec clean_ns(Ctx :: ctx(), Ns :: ns()) -> ctx().
clean_ns({?KEY_CTX, []}, _) ->
  {?KEY_CTX, []};
clean_ns({?KEY_CTX, [{{Ns, _}, _} | Bindings]}, Ns) ->
  clean_ns({?KEY_CTX, Bindings}, Ns);
clean_ns({?KEY_CTX, [Binding = {{_, _}, _} | Bindings]}, Ns) ->
  {?KEY_CTX, Bindings_} = clean_ns({?KEY_CTX, Bindings}, Ns),
  {?KEY_CTX, [Binding | Bindings_]}.

%%% @private Returns the existing namespace or the global one if it does not
%%%          exist.
-spec get_ns(Env :: env()) -> ns().
get_ns({?KEY_ENV, Env}) ->
  get_key(?KEY_NS, Env, {true, global}).

%%% @private Overwrites the namespace in the specified monitor environment with
%%%          the new one.
-spec set_ns(Env :: env(), Ns :: ns()) -> env().
set_ns({?KEY_ENV, Env}, {ns, Ns}) ->
  {?KEY_ENV, put_key(ns, Ns, Env)}.

%%% @private Copies the namespace from the environment of `From` to `To`. The
%%%          existing namespace is overwritten.
-spec copy_ns(From :: monitor(), To :: monitor()) -> monitor().
copy_ns(From, To) ->
  EnvTo = set_ns(get_env(To), get_ns(get_env(From))),
  set_env(To, EnvTo).

%%% @private Returns the value element of the specified tagged tuple.
-spec unwrap_value(Pair :: {atom(), any()}) -> any().
unwrap_value({_, Value}) ->
  Value;
unwrap_value(Any) ->
  io:format("The unwrapped value is : ~p~n", [Any]), % Kept as a check for now!
  ok.


%%% ----------------------------------------------------------------------------
%%% Private monitor and proof derivation display functions.
%%% ----------------------------------------------------------------------------

%%% @private Extends the current proof derivation ID with a new sub-derivation.
-spec new_pdid(Id :: list(integer())) -> list(integer()).
new_pdid(Id) when is_list(Id) ->
  [1 | Id].

%%% @private Increments the ID of the current proof derivation.
-spec inc_pdid(Id :: list(integer())) -> list(integer()).
inc_pdid([Idx | Idxs]) ->
  [Idx + 1 | Idxs].

%%% @private Returns the proof derivation ID as an IoList where each derivation
%%%          index is period-separated.
-spec pdid_to_iolist(Id :: list(integer())) -> iolist().
pdid_to_iolist(Id = [_ | _]) ->
  tl(lists:foldl(fun(Idx, Id) -> [$., integer_to_list(Idx) | Id] end, [], Id)).

%% @private Returns a human-parsable monitor representation of the specified
%%          monitor.
%%
%%          To correctly print the variable binding context as part of the
%%          monitor representation, the function relies on the fact that the
%%          derivation propagates the variable binding context downwards to
%%          each monitor reduction. This is needed since the monitor being
%%          printed is converted to human-parsable form dynamically on the fly,
%%          and the previous monitor reduction is not available, but "peeled
%%          away". Propagating the context ensures that binders instantiated to
%%          trace event values are preserved in monitor continuations.
-spec m_to_iolist(M :: monitor()) -> iolist().
m_to_iolist(M) ->
  {ctx, Ctx} = get_ctx(get_env(M)),
  Vars = [{Name, Value} || {{_, Name}, Value} <- Ctx],
  lists:flatten(io_lib:format("~s \e[0;33msub([\e[0m \e[37m~s\e[0m\e[0;33m])\e[0m", [m_to_iolist(M, Vars),
    [io_lib:format("~s=~w ", [Name, Value]) || {Name, Value} <- Vars]])).

%% @private Returns a human-parsable monitor representation using the specified
%%          variable binding context to substitute variables. For the time being
%%          this context is not used, but will be used in the future.
-spec m_to_iolist(M :: monitor(), Ctx :: list({atom(), any()})) -> iolist().
m_to_iolist({yes, Env = {env, _}}, _) ->
  unwrap_value(get_str(Env));
m_to_iolist({no, Env = {env, _}}, _) ->
  unwrap_value(get_str(Env));
m_to_iolist({var, Env = {env, _}, _}, _) ->
  unwrap_value(get_str(Env));
m_to_iolist({act, Env = {env, _}, _, M}, Ctx) ->

  % The continuation of an action is encoded as a function. In order to
  % stringify the rest of the monitor, this function needs to be applied so that
  % it is "peeled away" and the rest of the monitor can be accessed. Unfold the
  % continuation monitor function using dummy data obtained from the monitor
  % itself: this dummy data matches exactly by the function encoding the pattern
  % corresponding to the trace event. This dummy data does not interfere with
  % the constraint segment of the action, since constraints are not present in
  % the function encoding the monitor continuation body. Note that functions
  % encoding actions accept a single parameter.
%%  ?TRACE(">> Pat is: ~p", [get_pat(Env)]),
%%  ?TRACE(">> Unwrapped Pat is: ~p", [unwrap_value(get_pat(Env))]),
  M_ = M(unwrap_value(get_pat(Env))),

  [re:replace(unwrap_value(get_str(Env)), " when ", ","), $., m_to_iolist(M_, Ctx)];
m_to_iolist({chs, Env = {env, _}, M, N}, Ctx) ->
  [$(, m_to_iolist(M, Ctx), $ , unwrap_value(get_str(Env)), $ , m_to_iolist(N, Ctx), $)];
m_to_iolist({'or', Env = {env, _}, M, N}, Ctx) ->
  [m_to_iolist(M, Ctx), $ , unwrap_value(get_str(Env)), $ , m_to_iolist(N, Ctx)];
m_to_iolist({'and', Env = {env, _}, M, N}, Ctx) ->
  [m_to_iolist(M, Ctx), $ , unwrap_value(get_str(Env)), $ , m_to_iolist(N, Ctx)];
m_to_iolist({rec, Env = {env, _}, M}, Ctx) ->

  % The continuation of a recursive construct is encoded as a function. In order
  % to stringify the rest of the monitor, this function needs to be applied.
  % Recursive monitor definitions do not accept parameters.
  [unwrap_value(get_str(Env)), m_to_iolist(M(), Ctx)].

%% @private Formats the specified proof derivation list.
-spec format_pdlist(PdList :: list(pd())) -> {non_neg_integer(), iolist()}.
format_pdlist(PdList) ->
  lists:foldl(
    fun(Pd, {I, IoList}) ->
      {I - 1, [[io_lib:format("~n\e[4;32mDerivation ~w:\e[0m~n", [I]), format_pd(Pd)] | IoList]}
    end,
    {length(PdList), []}, PdList
  ).

%% TODO: Use the ?INDENT() parametrized macro.
%% @private Formats the specified proof derivation.
-spec format_pd(pd()) -> iolist().
format_pd({PdId, Rule, Act, M, M_}) ->
  Indent = length(PdId) + length(?PD_SEP),
  io_lib:format("~*s [~s, \e[1;36maxiom ~s\e[0m] ~s~n\e[0;36m~*s-(~w)->\e[0m~n~*s~s~n",
    [Indent, ?PD_SEP, pdid_to_iolist(PdId), Rule, m_to_iolist(M), Indent + 1, "", Act, Indent + 1, "", m_to_iolist(M_)]
  );
format_pd({PdId, Rule, Act, M, M_, {pre, PdM}}) -> % mChs
  PdMFmt = format_pd(PdM),
  Indent = length(PdId) + length(?PD_SEP),
  [io_lib:format("~*s [~s, \e[1;36mrule ~s\e[0m] ~s~n\e[0;36m~*s-(~w)->\e[0m~n~*s~s~n",
    [Indent, ?PD_SEP, pdid_to_iolist(PdId), Rule, m_to_iolist(M), Indent + 1, "", Act, Indent + 1, "", m_to_iolist(M_)])
    | PdMFmt
  ];
format_pd({PdId, Rule, Act, M, M_, N_, {pre, PdM}}) -> % mTauL and mTauR
  PdMFmt = format_pd(PdM),
  Indent = length(PdId) + length(?PD_SEP),
  [io_lib:format("~*s [~s, \e[1;36mrule ~s\e[0m] ~s~n\e[0;36m~*s-(~w)->\e[0m~n~*s ~s ~s ~s~n",
    [length(PdId) + 1, "-", pdid_to_iolist(PdId), Rule, m_to_iolist(M), Indent + 1, "", Act, Indent + 1, "", m_to_iolist(M_), unwrap_value(get_str(get_env(M))), m_to_iolist(N_)])
    | PdMFmt
  ];
format_pd({PdId, Rule, Act, M, M_, N_, {pre, PdM}, {pre, PdN}}) ->
  {PdMFmt, PdNFmt} = {format_pd(PdM), format_pd(PdN)},
  Indent = length(PdId) + length(?PD_SEP),
  [
    [
      io_lib:format("~*s [~s, \e[1;36mrule ~s\e[0m] ~s~n\e[0;36m~*s-(~w)->\e[0m~n~*s~s ~s ~s~n",
        [length(PdId) + 1, "-", pdid_to_iolist(PdId), Rule, m_to_iolist(M), Indent + 1, "", Act, Indent + 1, "", m_to_iolist(M_), unwrap_value(get_str(get_env(M))), m_to_iolist(N_)])
      | PdMFmt
    ]
    | PdNFmt
  ].


%%% ----------------------------------------------------------------------------
%%% Monitor instrumentation and weaving support functions.
%%% ----------------------------------------------------------------------------

%% @private Retrieves the monitor function stored in the process dictionary (if
%% any), and applies it on the event. The result is put back in the process
%% dictionary. If a verdict state is reached, the callback function is invoked,
%% otherwise nothing is done. When no monitor function is stored inside the
%% process dictionary (i.e. meaning that the process is not monitored), the atom
%% `undefined' is returned.
-spec do_monitor(Event, VerdictFun) -> monitor() | undefined
  when
  Event :: event:evm_event(),
  VerdictFun :: fun((Verdict :: verdict(), list(pd())) -> any()).
do_monitor(Event, VerdictFun) when is_function(VerdictFun, 2) ->
  case get(?MONITOR) of
    undefined ->
      ?TRACE("Analyzer undefined; discarding trace event ~w.", [Event]),
      undefined;
    {PdList, M} ->

      % Check whether the trace event should be recorded. We do this via the
      % environmental variable DEBUG.
      case os:getenv("DEBUG", undefined) of
        undefined ->
          ok;
        _ ->
          % Debug flag set. Forward trace event to writer.
          case whereis(event_writer) of
            undefined ->
              ?WARN(
                "Attempting to forward event ~p to event_writer which is down.
                Check that it is switched on.", [Event]);
            Pid ->
              ?TRACE("Forwarding event ~p to event_writer (~p).", [Event, Pid]),
              Pid ! {event, Event}
          end
      end,


      % Analyze event. At this point, monitor might have reached a verdict.
      % Check whether verdict is reached to enable immediate detection, should
      % this be the case.
%%      put(?MONITOR, {PdList_, M_} = analyze(Event, M, PdList)),
      put(?MONITOR, {_, M_} = analyze(Event, M, [])), % TODO: Use to discard the PdList and make monitor more space efficient.
      case is_verdict(M_) of
        true ->
          {Verdict, _} = M_,
%%          VerdictFun(Verdict, PdList_);
          VerdictFun(Verdict, []); % TODO: Use to discard the PdList and make monitor more space efficient.
        false ->
          ok
      end,
      M_
  end.

%% @doc Default filter that allows all events to pass.
-spec filter(Event :: event:int_event()) -> true.
filter(_) ->
  true. % True = keep event.

%% @private Determines whether the specified monitor is indeed a verdict.
-spec is_verdict(V :: {?VERDICT_YES | ?VERDICT_NO, env()}) -> boolean().
is_verdict({V, _}) when V =:= ?VERDICT_YES; V =:= ?VERDICT_NO ->
  true;
is_verdict(_) ->
  false.

%% @private Formats verdicts in the correct colors.
-spec format_verdict(Fmt, Args, M) -> ok
  when
  Fmt :: io:format(),
  Args :: list(),
  M :: verdict().
format_verdict(Fmt, Args, no) ->
  io:format(lists:flatten(["\e[1;31m:: Violation: ", Fmt, "\e[0m"]), Args);
format_verdict(Fmt, Args, yes) ->
  io:format(lists:flatten(["\e[1;32m:: Satisfaction: ", Fmt, "\e[0m"]), Args).

%% Tests.
%%{ok, M} = lin_7:m5().
%% lin_analyzer:embed(M).
%% lin_analyzer:dispatch({send, self(), self(), {1,3}}).
%% lin_analyzer:dispatch({send, self(), self(), {6,1}}).
%% lin_analyzer:dispatch({send, self(), self(), {2,2}}).

%% lin_analyzer:analyze_trace([{trace, self(), send, {1, 3}, self()}, {trace, self(), send, {2, 2}, self()}], M).

%% 1. lin_weaver:weave_file("/Users/duncan/Dropbox/PhD/Development/detecter/detecter/src/synthesis/calc_server.erl", fun prop_add_rec:mfa_spec/1, [{outdir, "/Users/duncan/Dropbox/PhD/Development/detecter/detecter/ebin"}]).
%% 2. calc_server:start(10).
%% 3. Pid ! {add, 1, 2}.


% Demo plan and web site example.
% Part 1.
% lin_weaver:weave_file("/Users/duncan/Dropbox/PhD/Development/detecter/detecter/src/synthesis/calc_server.erl", fun prop_add_rec:mfa_spec/1, [{outdir, "/Users/duncan/Dropbox/PhD/Development/detecter/detecter/ebin"}]).
% Pid = calc_server:start(10).
% Pid ! {self(), {add, 1, 3}}.

% Part 2.
% {ok, M} = prop_add_rec:mfa_spec({calc_server, loop, [10]}).
% lin_analyzer:analyze_trace([{send, self(), self(), {self(), {add, 1, 3}}}], M). % Should give yes.
% lin_analyzer:analyze_trace([{init, self(), self(), {calc_server, loop, [10]}}, {recv, self(), {self(), {add, 1, 3}}}, {send, self(), self(), 10}], M). Should give no.