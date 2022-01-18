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
-module(lin_7).
-author("Duncan Paul Attard").

-compile(export_all).

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([]).

%%% Callbacks/Internal.
-export([]).

%%% Types.
-export_type([]).

%%% Implemented behaviors.
%-behavior().


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------


%%% Monitor environment keys.
-define(KEY_ENV, env).
-define(KEY_ENV_STR, str).
-define(KEY_ENV_VAR, var).
-define(KEY_ENV_DVARS, vars).
-define(KEY_ENV_PDID, pdid).
-define(KEY_ENV_CTX, ctx).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

% P2: Two consecutive actions cannot be the same.
%
% max(X.[a,true]([b,a=:=b]ff and X))
%
% Aim: To test the general behaviour of one recursive operator, and in
% particular, that recursive variables are correctly expanded. One other aspect
% that is tested is the automatic reduction of the monitor when tau transitions
% can be performed. The monitor is reduced to a state where it is ready to
% analyze the next action.
%
% The resulting monitor from this property should be:
% rec (x.a,true.((b,a=:=b.no + b,a=/=b.yes) and x ) + a,false.yes)
%
% which can transition to no given the trace 1.2.2 in 8 reductions, ending with
% tha application of axiom mConN.
m1() ->
  {ok,
    {rec,
      {env, [{str, "rec X"}]},
      fun X() ->
        {chs,
          {env, [{str, "+"}]},
          {act,
%%            {env, [{str, "{:A} when true"}, {var, 'A'}, {ctx, [{'A', undef}]}]},
            {env, [{str, "{:A} when true"}, {var, 'A'}]},
            fun(A) -> true; (_) -> false end,
            fun(A) ->
              {'and',
                {env, [{str, "and"}]},
                {chs,
                  {env, [{str, "+"}]},
                  {act,
%%                    {env, [{str, "{:B} when {:A} =:= {:B}"}, {var, 'B'}, {ctx, [{'A', undef}, {'B', undef}]}]},
                    {env, [{str, "{:B} when {:A} =:= {:B}"}, {var, 'B'}]},
                    fun(B) when A =:= B -> true; (_) -> false end,
                    fun(B) ->
                      {no, {env, [{str, "no"}]}}
                    end
                  },
                  {act,
%%                    {env, [{str, "{:B} when not({:A} =:= {:B})"}, {var, 'B'}, {ctx, [{'A', undef}, {'B', undef}]}]},
                    {env, [{str, "{:B} when not({:A} =:= {:B})"}, {var, 'B'}]},
                    fun(B) when A =:= B -> false; (_) -> true end,
                    fun(B) ->
                      {yes, {env, [{str, "yes"}]}}
                    end
                  }
                },
                {var, {env, [{str, "X"}]}, X}
              }
            end
          },
          {act,
%%            {env, [{str, "{:A} when not(true)"}, {var, 'A'}, {ctx, [{'A', undef}]}]},
            {env, [{str, "{:A} when not(true)"}, {var, 'A'}]},
            fun(A) -> false; (_) -> true end,
            fun(A) ->
              {yes, {env, [{str, "yes"}]}}
            end
          }
        }
      end
    }
  }.

% Property P2: The first action is unique.
%
% maxHML = [a,true]max(X.[b,a=:=b]ff and [b,a=/=b]X).
%
% Monitor = a,true.rec(x,(b,a=:=b.no + b,not(a=:=b).yes) and (b,a=/=b.x + b,not(a=/=b).yes)) + a,not(true).yes
%
% Used to test that the data context is preserved across recursion with taus.
m2() ->
  {ok,
    {chs,
      {env, [{str, "+"}]},
      {act,
        {env, [{str, "{:A} when true"}, {var, 'A'}]},
        fun(A) -> true; (_) -> false end,
        fun(A) ->
          {rec,
            {env, [{str, "rec X"}]},
            fun X() ->
              {'and',
                {env, [{str, "and"}]},
                {chs,
                  {env, [{str, "+"}]},
                  {act,
                    {env, [{str, "{:B} when {:A}=:={:B}"}, {var, 'B'}]},
                    fun(B) when A =:= B -> true; (_) -> false end,
                    fun(B) ->
                      {no, {env, [{str, "no"}]}}
                    end
                  },
                  {act,
                    {env, [{str, "{:B} when not({:A}=:={:B})"}, {var, 'B'}]},
                    fun(B) when A =:= B -> false; (_) -> true end,
                    fun(B) ->
                      {yes, {env, [{str, "yes"}]}}
                    end
                  }
                },
                {chs,
                  {env, [{str, "+"}]},
                  {act,
                    {env, [{str, "{:B} when {:A}=/={:B}"}, {var, 'B'}]},
                    fun(B) when A =/= B -> true; (_) -> false end,
                    fun(B) ->
                      {var, {env, [{str, "X"}]}, X}
                    end
                  },
                  {act,
                    {env, [{str, "{:B} when not({:A}=/={:B})"}, {var, 'B'}]},
                    fun(B) when A =/= B -> false; (_) -> true end,
                    fun(B) ->
                      {yes, {env, [{str, "yes"}]}}
                    end
                  }
                }
              }
            end
          }
        end
      },
      {act,
        {env, [{str, "{:A} when not(true)"}, {var, 'A'}]},
        fun(A) -> false; (_) -> true end,
        fun(A) ->
          {yes, {env, [{str, "yes"}]}}
        end
      }
    }
  }.

% Property P3: Not a great one, but tests that the data context is preserved
% during tauLs and tauRs.
%
% maxHML = [a,true]max(X.[b,a=:=b]ff and X).
%
% Monitor: a,true.rec(X. (b,a=:=b.no + b,not(a=:=b).yes) and X) + a,not(true).yes
m3() ->
  {ok,
    {chs,
      {env, [{str, "+"}]},
      {act,
        {env, [{str, "{:A} when true"}, {var, 'A'}]},
        fun(A) -> true; (_) -> false end,
        fun(A) ->
          {rec,
            {env, [{str, "rec X"}]},
            fun X() ->
              {'and',
                {env, [{str, "and"}]},
                {chs,
                  {env, [{str, "+"}]},
                  {act,
                    {env, [{str, "{:B} when {:A}=:={:B}"}, {var, 'B'}]},
                    fun(B) when A =:= B -> true; (_) -> false end,
                    fun(B) ->
                      {no, {env, [{str, "no"}]}}
                    end
                  },
                  {act,
                    {env, [{str, "{:B} when not({:A}=:={:B})"}, {var, 'B'}]},
                    fun(B) when A =:= B -> false; (_) -> true end,
                    fun(B) ->
                      {yes, {env, [{str, "yes"}]}}
                    end
                  }
                },
                {var, {env, [{str, "X"}]}, X}
              }
            end
          }
        end
      },
      {act,
        {env, [{str, "{:A} when not(true)"}, {var, 'A'}]},
        fun(A) -> false; (_) -> true end,
        fun(A) ->
          {yes, {env, [{str, "yes"}]}}
        end
      }
    }
  }.

% TODO: Consider removing the unbound variables in the list of tuples 'bind',
% TODO: as these are not required actually, and we can add them on the fly when
% TODO: we are evaluating the monitor.
% TODO: Actually I think that the variable suffices, since when we are
% TODO: generating the instantiated monitor string, the string is already
% TODO: instantiated with the value.

%%m_str() ->
%%  {ok,
%%    {rec,
%%      fun X() ->
%%        {var, }
%%      end}
%%    }.


% The ID can be saved in the Env probably. The env is used or verbosing and IDs!

% Proof derivation strategy for rules that transition via the internal action tau .
derive_tau(L = {'and', Env, {yes, _}, M}, PdId) ->

%%  ?DEBUG(":: (~s) Reducing using axiom mConYL: ~s.", [str_pdid(PdId), get_str(Env)]),
  ?DEBUG(":: (~s) Reducing using axiom mConYL: ~s.", [str_pdid(PdId), to_iolist(L)]),

  % Axiom mConYL.
  {true, {{PdId, mConYL, tau, {'and', Env}}, copy_ctx(L, M)}};

derive_tau(L = _@M = {'and', Env, M, {yes, _}}, PdId) ->

%%  ?DEBUG(":: (~s) Reducing using axiom mConYR: ~s.", [str_pdid(PdId), get_str(Env)]),
  ?DEBUG(":: (~s) Reducing using axiom mConYR: ~s.", [str_pdid(PdId), to_iolist(_@M)]),

  % Axiom mConYR.
  {true, {{PdId, mConYR, tau, {'and', Env}}, copy_ctx(L, M)}};

derive_tau(_@M = {'and', Env, No = {no, _}, _}, PdId) ->
%%  ?DEBUG(":: (~s) Reducing using axiom mConNL: ~s.", [str_pdid(PdId), get_str(Env)]),
  ?DEBUG(":: (~s) Reducing using axiom mConNL: ~s.", [str_pdid(PdId), to_iolist(_@M)]),

  % Axiom mConNL.
  {true, {{PdId, mConNL, tau, {'and', Env}}, No}};

derive_tau(_@M = {'and', Env, _, No = {no, _}}, PdId) ->
%%  ?DEBUG(":: (~s) Reducing using axiom mConNR: ~s.", [str_pdid(PdId), get_str(Env)]),
  ?DEBUG(":: (~s) Reducing using axiom mConNR: ~s.", [str_pdid(PdId), to_iolist(_@M)]),

  % Axiom mConNR.
  {true, {{PdId, mConNR, tau, {'and', Env}}, No}};

derive_tau(_@M = {'or', Env, Yes = {yes, _}, _}, PdId) ->
%%  ?DEBUG(":: (~s) Reducing using axiom mDisYL: ~s.", [str_pdid(PdId), get_str(Env)]),
  ?DEBUG(":: (~s) Reducing using axiom mDisYL: ~s.", [str_pdid(PdId), to_iolist(_@M)]),

  % Axiom mDisYL.
  {true, {{PdId, mDisYL, tau, {'or', Env}}, Yes}};

derive_tau(_@M = {'or', Env, _, Yes = {yes, _}}, PdId) ->
%%  ?DEBUG(":: (~s) Reducing using axiom mDisYR: ~s.", [str_pdid(PdId), get_str(Env)]),
  ?DEBUG(":: (~s) Reducing using axiom mDisYR: ~s.", [str_pdid(PdId), to_iolist(_@M)]),

  % Axiom mDisYR.
  {true, {{PdId, mDisYR, tau, {'or', Env}}, Yes}};

derive_tau(L = _@M = {'or', Env, {no, _}, M}, PdId) ->
%%  ?DEBUG(":: (~s) Reducing using axiom mDisNL: ~s.", [str_pdid(PdId), get_str(Env)]),
  ?DEBUG(":: (~s) Reducing using axiom mDisNL: ~s.", [str_pdid(PdId), to_iolist(_@M)]),

  % Axiom mDisNL.
  {true, {{PdId, mDisNL, tau, {'or', Env}}, copy_ctx(L, M)}};

derive_tau(L = _@M = {'or', Env, M, {no, _}}, PdId) ->
%%  ?DEBUG(":: (~s) Reducing using axiom mDisNR: ~s.", [str_pdid(PdId), get_str(Env)]),
  ?DEBUG(":: (~s) Reducing using axiom mDisNR: ~s.", [str_pdid(PdId), to_iolist(_@M)]),


  % Axiom mDisNR.
  {true, {{PdId, mDisNR, tau, {'or', Env}}, copy_ctx(L, M)}};

derive_tau(L = _@M = {rec, Env = {env, _}, M}, PdId) ->
%%  ?DEBUG(":: (~s) Reducing using axiom mRec: ~s.", [str_pdid(PdId), get_str(Env)]),
  ?DEBUG(":: (~s) Reducing using axiom mRec: ~s.", [str_pdid(PdId), to_iolist(_@M)]),

  % Axiom mRec.
  M_ = M(),


%%  {true, {{PdId, mRec, tau, {rec, Env}}, M_}};
  {true, {{PdId, mRec, tau, {rec, Env}}, copy_ctx(L, M_)}};

derive_tau(L = _@M = {var, Env = {env, _}, M}, PdId) ->
%%  ?DEBUG(":: (~s) Reducing using axiom mRec: ~s.", [str_pdid(PdId), get_str(Env)]),
  ?DEBUG(":: (~s) Reducing using axiom mRec (var): ~s.", [str_pdid(PdId), to_iolist(_@M)]),

  % This case is the second part to the mRec rule which applies the recursive
  % variable that itself, is the function that we want to recurse on.

  % Axiom mRec.
  M_ = M(),
%%  {true, {{PdId, mRec, tau, {var, Env}}, M_}};
  {true, {{PdId, mRec, tau, {var, Env}}, copy_ctx(L, M_)}};

derive_tau(L = _@M = {Op, Env = {env, _}, M, N}, PdId) when Op =:= 'and'; Op =:= 'or' ->


  ?DEBUG(":: (~s) Trying to reduce using rule mTauL: ~s.", [str_pdid(PdId), to_iolist(_@M)]),
%%  case derive_tau(M, new_pdid(PdId)) of
  case derive_tau(copy_ctx(L, M), new_pdid(PdId)) of
    false ->
      ?DEBUG(":: (~s) Trying to reduce using rule mTauR: ~s.", [str_pdid(PdId), to_iolist(_@M)]),
%%      case derive_tau(N, new_pdid(PdId)) of
      case derive_tau(copy_ctx(L, N), new_pdid(PdId)) of
        false ->
          ?DEBUG(":: Unable to reduce futher using tau: ~s.", [to_iolist(_@M)]),
          false;
        {true, {PdN, N_}} ->
%%          ?DEBUG(":: (~s) Reducing using rule mTauR: ~s.", [str_pdid(PdId), to_iolist(_@M)]),
%%          {true, {{PdId, mTauR, tau, Env, {pd, PdN}}, {Op, Env, M, N_}}},
          {true, {{PdId, mTauR, tau, {Op, Env}, {pd, PdN}}, {Op, Env, M, N_}}}
      end;
    {true, {PdM, M_}} ->
%%      ?DEBUG(":: (~s) Reducing using rule mTauL: ~s.", [str_pdid(PdId), to_iolist(_@M)]),
%%      {true, {{PdId, mTauL, tau, Env, {pd, PdM}}, {Op, element(2, M_) ++ " " ++ atom_to_list(Op) ++ " " ++ element(2, N), M_, N}}}
      {true, {{PdId, mTauL, tau, {Op, Env}, {pd, PdM}}, {Op, Env, M_, N}}}
  end;

derive_tau(_, _) ->
  false.


% Proof derivation strategy for rules that transition via external actions.
derive_act(Act, _@M = M = {V, Env}, PdId) when V =:= yes; V =:= no ->
  ?assertNot(Act =:= tau),
%%  ?DEBUG(":: (~s) Reducing using axiom mVrd: ~s.", [str_pdid(PdId), get_str(Env)]),
  ?DEBUG(":: (~s) Reducing using axiom mVrd: ~s.", [str_pdid(PdId), to_iolist(_@M)]),

  % Axiom mVrd.
  {{PdId, mVrd, Act, {V, Env}}, M};

derive_act(Act, L = MM = {act, Env, C, M}, PdId) ->
  ?assertNot(Act =:= tau),
  ?assert(C(Act)),
  ?assert(is_function(M, 1)),
%%  ?DEBUG(":: (~s) Reducing using rule mAct: ~s.", [str_pdid(PdId), get_str(Env)]),


  % Get the variable binder for this action.
  Binder = unwrap_value(get_var(Env)),

  % Add the new binder also to the context of the current monitor so that
  % we can return it in the proof derivation.

%%  Ctx = get_ctx(Env),
%%  ?TRACE("Context of current monitor: ~p", [Ctx]),




  Ctx = new_binder(get_ctx(Env), Binder, Act),
  NewEnv = set_ctx(Env, Ctx),
  MMM = set_env(MM, NewEnv),

%%  ?TRACE("Current monitor with context updated: ~p", [MMM]),
  ?DEBUG(":: (~s) Reducing using rule mAct: ~s.", [str_pdid(PdId), to_iolist(MMM)]),


  % Axiom mAct.
  M_ = M(Act),
  ?assertNot(is_function(M_)),

  % The environment of M cannot be updated prior to applying M to Act, since M
  % is a function. Once M is applied, the new variable binding acquired during
  % the analysis of Act can be passed down to the unwrapped monitor by updating
  % its environment.
%%  Env_ = get_env(M_),
%%  Ctx_ = new_binder(get_ctx(Env_), Binder, Act),
%%  NewM = set_env(M_, set_ctx(Env_, Ctx_)),
  NewM = copy_ctx(MMM, M_),


  {{PdId, mAct, Act, {act, NewEnv}}, NewM}; % Updated monitor env.
%%  {{PdId, mAct, Act, {act, NewEnv}}, M_}; % Updated monitor env.

derive_act(Act, L = _@M = {chs, Env = {env, _}, M, N}, PdId) ->
  ?assert(is_tuple(M) andalso element(1, M) =:= act),
  ?assert(is_tuple(N) andalso element(1, N) =:= act),

  % Extend variable binding context of current monitor to the two sub-monitors
  % M and N.
%%  Ctx = get_ctx(Env),
%%  NewM = set_env(M, set_ctx(get_env(M), Ctx)),
%%  NewN = set_env(N, set_ctx(get_env(N), Ctx)),


%%  case {can_act(Act, NewM), can_act(Act, NewN)} of
%%  case {can_act(Act, M), can_act(Act, N)} of
  case {can_act(Act, M), can_act(Act, N)} of
    {true, false} ->
%%      ?DEBUG(":: (~s) Reducing using rule mChsL: ~s.", [str_pdid(PdId), get_str(Env)]),
      ?DEBUG(":: (~s) Reducing using rule mChsL: ~s.", [str_pdid(PdId), to_iolist(_@M)]),

      % Rule mChsL.
      {PdM_, M_} = derive_act(Act, copy_ctx(L, M), new_pdid(PdId)),
%%      {PdM_, M_} = derive_act(Act, NewM, new_pdid(PdId)),
      {{PdId, mChsL, Act, {chs, Env}, {pd, PdM_}}, M_};

    {false, true} ->
%%      ?DEBUG(":: (~s) Reducing using rule mChsR: ~s.", [str_pdid(PdId), get_str(Env)]),
      ?DEBUG(":: (~s) Reducing using rule mChsR: ~s.", [str_pdid(PdId), to_iolist(_@M)]),

      % Rule mChsR.
      {PdN_, N_} = derive_act(Act, copy_ctx(L, N), new_pdid(PdId)),
%%      {PdN_, N_} = derive_act(Act, NewN, new_pdid(PdId)),
      {{PdId, mChsR, Act, {chs, Env}, {pd, PdN_}}, N_}
  end;



derive_act(Act, L = _@M = {Op, Env = {env, _}, M, N}, PdId) when Op =:= 'and'; Op =:= 'or' ->
  ?assertNot(Act =:= tau),
%%  ?DEBUG(":: (~s) Reducing using rule mPar: ~s.", [str_pdid(PdId), get_str(Env)]),
  ?DEBUG(":: (~s) Reducing using rule mPar: ~s.", [str_pdid(PdId), to_iolist(_@M)]),


  % We need to pass on the current binding context to the next monitor. How?

  % 1. Get the environment of the next monitor.

  % 2. Get the binding context of the current monitor from its environment.

  % 3. Overwrite the (empty) binding context of the next monitor in its environment.

  % 4. Overwrite the environment of the current monitor.

  % 5. Recurse.


  % Extend variable binding context of current monitor to the two sub-monitors
  % M and N.
%%  Ctx = get_ctx(Env),
%%  NewM = set_env(M, set_ctx(get_env(M), Ctx)),
%%  NewN = set_env(N, set_ctx(get_env(N), Ctx)),

%%  ?TRACE("New monitor M updated with new context: ~p", [NewM]),
%%  ?TRACE("New monitor N updated with new context: ~p", [NewN]),


  % Unfold respective sub-monitors. Proof derivation ID for second monitor N is
  % incremented accordingly.
  {PdM_, M_} = derive_act(Act, copy_ctx(L, M), new_pdid(PdId)),
  {PdN_, N_} = derive_act(Act, copy_ctx(L, N), inc_pdid(new_pdid(PdId))),


%%  {{PdM, M_}, {PdN, N_}} = {derive_act(Act, M, new_pdid(PdId)), derive_act(Act, N, inc_pdid(new_pdid(PdId)))},
%%  {{PdId, mPar, Act, Env, {pd, PdM}, {pd, PdN}}, {Op, element(2, M_) ++ " " ++ atom_to_list(Op) ++ " " ++ element(2, N_), M_, N_}}.
  {{PdId, mPar, Act, {Op, Env}, {pd, PdM_}, {pd, PdN_}}, {Op, Env, M_, N_}}.
% The env should be the updated Env, always.

%%extend_ctx(From, To) ->
%%  Ctx = get_ctx(Env),
%%  set_env(M, set_ctx(get_env(M), Ctx)).

can_act(Act, {act, _Env, C, _M}) ->
  ?assert(is_function(_M, 1)),
  C(Act).



reduce_tau(M, PdLst) ->
%%  ?TRACE("[ Starting a new derivation for monitor on action 'tau' ]"),

  case derive_tau(M, new_pdid([])) of
    false ->

      % No more tau reductions.
      {PdLst, M};
    {true, {PdM, M_}} ->

      % Monitor state reduced by one tau transition. Attempt to reduce further.
      reduce_tau(M_, [PdM | PdLst])
  end.

% Assumes that the monitor is already in a ready state.
analyze(Act, M, PdLst) ->
%%  ?TRACE("[ Starting a new derivation for monitor on action '~p' ]", [Act]),

  % Analyze action.
  {PdM, M_} = derive_act(Act, M, new_pdid([])),
%%  {PdM, M_} = derive_act(Act, M, new_pdid([]), []),

  % Check whether the residual monitor state can be reduced further using tau
  % transitions. This ensures that the monitor is always left in a state where
  % it is ready to analyse the next action.
  reduce_tau(M_, [PdM | PdLst]).

%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------


% Proof derivation ID related.
new_pdid(Id) when is_list(Id) ->
  [1 | Id].

inc_pdid([Idx | Idxs]) ->
  [Idx + 1 | Idxs].

str_pdid(Id = [_ | _]) ->
  tl(lists:foldl(fun(Idx, Id) -> [$., integer_to_list(Idx) | Id] end, [], Id)).


% Generic.
%%get_key(List, Key) when is_list(List) ->
%%  case lists:keyfind(Key, 1, List) of
%%    false ->
%%      false;
%%    {Key, Val} ->
%%      Val
%%  end.

%%put_key(List, Key, Value) when is_list(List) ->
%%  lists:keystore(Key, 1, List, {Key, Value}).

% Monitor environment related.
%%{env, [{str, "{:A} when true"}, {vars, [{'A', undef}]}]},
%%get_env(Node) when is_tuple(Node), tuple_size(Node) >= 2 ->
%%  {?KEY_ENV, Env} = element(2, Node),
%%  Env.

% TODO: Important! The Return value of these should be env!

%%put_dvar({env, Env}, Name, Value) when is_list(Env) ->
%%
%%  Vars = case get_key(Env, ?KEY_ENV_DVARS) of false -> []; Vars -> Vars end,
%%  {env, put_key(Env, ?KEY_ENV_DVARS, put_key(Vars, Name, Value))}.
%%  case get_key(Env, ?KEY_ENV_DVARS) of
%%    false ->
%%      false; % ERROR!
%%    Vars ->
%%      {env, put_key(Env, ?KEY_ENV_DVARS, put_key(Vars, Name, Value))}
%%  end.

%%new_binding({env, Env}, Name, Value) ->
%%
%%  Vars =
%%    case get_key(Env, ?KEY_ENV_DVARS) of
%%      false ->
%%        [];
%%      Vars ->
%%        Vars
%%    end,
%%  {env, put_key(Env, ?KEY_ENV_DVARS, put_key(Vars, Name, Value))}.


%%get_dvar({env, Env}, Name) when is_list(Env) ->
%%  case get_key(Env, ?KEY_ENV_DVARS) of
%%    false ->
%%      false;
%%    Vars ->
%%      get_key(Vars, Name)
%%  end.


%%get_rvar(Env) ->
%%  get_key(Env, ?KEY_ENV_RVAR).

%%get_vars({env, Env}) ->
%%  case get_key(Env, ?KEY_ENV_DVARS) of false -> .

%%put_id({env, Env}, Value) ->
%%  put_key(Env, ?KEY_ENV_PDID, Value).

%%get_var({env, Env}) ->
%%  get_key(Env, ?KEY_ENV_VAR).



get_key(Key, List, false) ->
  lists:keyfind(Key, 1, List);


get_key(Key, List, {true, Default}) ->
  case get_key(Key, List, false) of
    false ->
      {Key, Default};
    Pair = {Key, _} ->
      Pair
  end.

put_key(Key, Value, List) ->
  lists:keystore(Key, 1, List, {Key, Value}).


get_env(M) when is_tuple(M), tuple_size(M) > 1 ->
  {env, _} = element(2, M).

set_env(M, {env, Env}) when is_tuple(M), tuple_size(M) > 1, is_list(Env) ->
  setelement(2, M, {env, Env}).


% Returns the string.
get_str({env, Env}) when is_list(Env) ->
  get_key(?KEY_ENV_STR, Env, false).

% Returns the variable.
get_var({env, Env}) when is_list(Env) ->
  get_key(?KEY_ENV_VAR, Env, false).


% Returns the existing context or an empty one if none exists.
get_ctx({env, Env}) when is_list(Env) ->
  get_key(?KEY_ENV_CTX, Env, {true, []}).

set_ctx({env, Env}, {ctx, Ctx}) when is_list(Env), is_list(Ctx) ->
  {env, put_key(?KEY_ENV_CTX, Ctx, Env)}.

% Returns updated environment.
%%new_binder({env, Env}, Name, Value) ->
%%  {?KEY_ENV_CTX, Ctx} = get_ctx({env, Env}),
%%  {env, put_key(?KEY_ENV_CTX, put_key(Name, Value, Ctx), Env)}.

new_binder({ctx, Ctx}, Name, Value) when is_list(Ctx) ->
  {ctx, put_key(Name, Value, Ctx)}.



copy_ctx(From, To) ->
  EnvTo = set_ctx(get_env(To), get_ctx(get_env(From))),
  set_env(To, EnvTo).


unwrap_value({_, Value}) ->
  Value.

% Also needs to pass on context.

% Stringifies the monitor.
to_iolist({yes, Env = {env, _}}) ->
%%  ?TRACE("Visting yes."),
  unwrap_value(get_str(Env));
to_iolist({no, Env = {env, _}}) ->
%%  ?TRACE("Visiting no"),
  unwrap_value(get_str(Env));
to_iolist({var, Env = {env, _}, _}) ->
%%  ?TRACE("Visiting rec var"),
  unwrap_value(get_str(Env));
to_iolist(L = {act, Env = {env, _}, _, M}) ->

  M_ = M(undef),
  MM_ = copy_ctx(L, M_),

%%  ?TRACE("Visiting act with context: ~p", [get_ctx(Env)]),
%%  ?TRACE("Visiting act and getting string: ~p", [get_str(Env)]),
%%  ?TRACE("Visiting act"),
  [
    [format_ph(unwrap_value(get_str(Env)), unwrap_value(get_ctx(Env)))],
    ".", to_iolist(MM_)
  ];

to_iolist(L = {chs, Env = {env, _}, M, N}) ->
%%  ?TRACE("Visiting chs"),


%%  ["(", to_iolist(M), " ", unwrap_value(get_str(Env)), " ", to_iolist(N) ++ ")"];
  ["(", to_iolist(copy_ctx(L, M)), " ", unwrap_value(get_str(Env)), " ", to_iolist(copy_ctx(L, N)), ")"];
to_iolist(L = {'or', Env = {env, _}, M, N}) ->
%%  ?TRACE("Visiting or"),
%%  [to_iolist(M), " ", unwrap_value(get_str(Env)), " ", to_iolist(N)];




  [to_iolist(copy_ctx(L, M)), " ", unwrap_value(get_str(Env)), " ", to_iolist(copy_ctx(L, N))];
to_iolist(L = {'and', Env = {env, _}, M, N}) ->
%%  ?TRACE("Visiting and"),
%%  [to_iolist(M), " ", unwrap_value(get_str(Env)), " ", to_iolist(N)];



  [to_iolist(copy_ctx(L, M)), " ", unwrap_value(get_str(Env)), " ", to_iolist(copy_ctx(L, N))];
to_iolist(L = {rec, Env = {env, _}, M}) ->
%%  ?TRACE("Visiting rec"),
  M_ = M(),
%%  [unwrap_value(get_str(Env)), "(", to_iolist(M()), ")"].
  [unwrap_value(get_str(Env)), "(", to_iolist(copy_ctx(L, M_)), ")"].


% General implementation of format placeholder.
format_ph(IoList, []) ->
  IoList;
format_ph(IoList, [{Name, Value} | Vars]) ->
  format_ph(format_ph(IoList, Name, Value), Vars).

% Format single place holder.
format_ph(IoList, Name, Value) ->
  Pat = [${, $:, atom_to_list(Name), $}],
  case re:run(IoList, Pat, [global]) of
    nomatch ->
      IoList;
    {match, Matches} ->
      Format = re:replace(IoList, Pat, "~p", [global, {return, list}]),
      io_lib:format(Format, lists:duplicate(length(Matches), Value))
  end.

