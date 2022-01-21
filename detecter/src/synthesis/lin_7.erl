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

-define(PD_SEP, "-").

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

% P2: Two consecutive actions cannot be the same.
%
% max(X.[a,true]([b,a=:=b]ff and X)) [a]

% [a,true]([b,1=:=b]ff and max(X.[a,true]([b,a=:=b]ff and X)))
% -(1)-> [b,a=:=b]ff and max(X.[a,true]([b,a=:=b]ff and X))) GOOD [a->1]
% -(1)-> [b,1=:=b]ff and max(X.[1,true]([b,1=:=b]ff and X))) BAD
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
      {env, [{str, "rec X"}, {var, 'X'}]},
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
%%                      io:format("Reached verdict no.~n"),
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
                {var, {env, [{str, "X"}, {var, 'X'}]}, X}
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
            {env, [{str, "rec X"}, {var, 'X'}]},
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
                      {var, {env, [{str, "X"}, {var, 'X'}]}, X}
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


% Property P4: All actions are unique.
%
% Formula F4: max(X.[a,true](max(Y.[b,a=:=b]ff and [b,a=/=b]Y) and X))
%
% Monitor M4: rec(X. a,true(rec(Y. (b,a=:=b.no + b,not(a=:=b).yes) and (b,a=/=b.Y + b,a=/=b.yes)) and X) + a,not(true).yes)
m4() ->
  {ok,
    {rec,
      {env, [{str, "rec X"}, {var, 'X'}]},
      fun X() ->
        {chs,
          {env, [{str, "+"}]},
          {act,
            {env, [{str, "{:A} when true"}, {var, 'A'}]},
            fun(A) -> true; (_) -> false end,
            fun(A) ->
              {'and',
                {env, [{str, "and"}]},
                {rec,
                  {env, [{str, "rec Y"}, {var, 'Y'}]},
                  fun Y() ->
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
                            {var, {env, [{str, "Y"}, {var, 'Y'}]}, Y}
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
                },
                {var, {env, [{str, "X"}, {var, 'X'}]}, X}
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
      end
    }
  }.

% P2: Two consecutive actions cannot be the same.
%
% max(X.[a,true]([b,a=:=b]ff and X)) [a]
%
% Instantiated to: the addition of the payload in two successive send actions
% cannot be equal.

% [a,true]([b,1=:=b]ff and max(X.[a,true]([b,a=:=b]ff and X)))
% -(1)-> [b,a=:=b]ff and max(X.[a,true]([b,a=:=b]ff and X))) GOOD [a->1]
% -(1)-> [b,1=:=b]ff and max(X.[1,true]([b,1=:=b]ff and X))) BAD
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
m5() ->
  {ok,
    {rec,
      {env, [{str, "rec X"}, {var, 'X'}]},
      fun X() ->
        {chs,
          {env, [{str, "+"}]},
          {act,
            {env, [{str, "{:A} when true"}, {var, 'A'}, {pat, {trace, undefined, send, {undefined, undefined}, undefined}}]},
            fun(_@A = {trace, _, send, {P, Q}, _}) -> true; (_) -> false end,
            fun(_@A = {trace, _, send, {P, Q}, _}) ->
              {'and',
                {env, [{str, "and"}]},
                {chs,
                  {env, [{str, "+"}]},
                  {act,
                    {env, [{str, "{:B} when {:A} =:= {:B}"}, {var, 'B'}, {pat, {trace, undefined, send, {undefined, undefined}, undefined}}]},
                    fun(_@B= {trace, _, send, {R, S}, _}) when P + Q =:= R + S -> true; (_) -> false end,
                    fun(_@B= {trace, _, send, {R, S}, _}) ->
%%                      io:format("Reached verdict no.~n"),
                      {no, {env, [{str, "no"}]}}
                    end
                  },
                  {act,
                    {env, [{str, "{:B} when not({:A} =:= {:B})"}, {var, 'B'}, {pat, {trace, undefined, send, {undefined, undefined}, undefined}}]},
                    fun(_@B= {trace, _, send, {R, S}, _}) when P + Q =:= R + S -> false; (_) -> true end,
                    fun(_@B= {trace, _, send, {R, S}, _}) ->
                      {yes, {env, [{str, "yes"}]}}
                    end
                  }
                },
                {var, {env, [{str, "X"}, {var, 'X'}]}, X}
              }
            end
          },
          {act,
            {env, [{str, "{:A} when not(true)"}, {var, 'A'}, {pat, {trace, undefined, send, {undefined, undefined}, undefined}}]},
            fun(_@A = {trace, _, send, {_, _}, _}) -> false; (_) -> true end,
            fun(_@A = {trace, _, send, {_, _}, _}) ->
              {yes, {env, [{str, "yes"}]}}
            end
          }
        }
      end
    }
  }.



% TODO: Consider removing the unbound variables in the list of tuples 'bind',
% TODO: as these are not required actually, and we can add them on the fly when
% TODO: we are evaluating the monitor.
% TODO: Actually I think that the variable suffices, since when we are
% TODO: generating the instantiated monitor string, the string is already
% TODO: instantiated with the value.


% The ID can be saved in the Env probably. The env is used or verbosing and IDs!

% Proof derivation strategy for rules that transition via the internal action tau .
derive_tau(L = {'and', _, {yes, _}, M}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mConYL: ~s.", [str_pdid(PdId), m_to_iolist(L)]),

  % Axiom mConYL.
%%  {true, {{PdId, mConYL, tau, L}, copy_ctx(L, M)}};
%%  {true, {{PdId, mConYL, tau, L, copy_ctx(L, M)}, copy_ctx(L, M)}};
  {true, {{PdId, mConYL, tau, L, copy_ns(L, copy_ctx(L, M))}, copy_ns(L, copy_ctx(L, M))}};

derive_tau(L = {'and', _, M, {yes, _}}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mConYR: ~s.", [str_pdid(PdId), m_to_iolist(L)]),

  % Axiom mConYR.
%%  {true, {{PdId, mConYR, tau, L}, copy_ctx(L, M)}};
%%  {true, {{PdId, mConYR, tau, L, copy_ctx(L, M)}, copy_ctx(L, M)}};
  {true, {{PdId, mConYR, tau, L, copy_ns(L, copy_ctx(L, M))}, copy_ns(L, copy_ctx(L, M))}};

derive_tau(L = {'and', _, No = {no, _}, _}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mConNL: ~s.", [str_pdid(PdId), m_to_iolist(L)]),

  % Axiom mConNL.
%%  {true, {{PdId, mConNL, tau, L}, No}};
  {true, {{PdId, mConNL, tau, L, No}, No}};

derive_tau(L = {'and', _, _, No = {no, _}}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mConNR: ~s.", [str_pdid(PdId), m_to_iolist(L)]),

  % Axiom mConNR.
%%  {true, {{PdId, mConNR, tau, L}, No}};
  {true, {{PdId, mConNR, tau, L, No}, No}};

derive_tau(L = {'or', _, Yes = {yes, _}, _}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mDisYL: ~s.", [str_pdid(PdId), m_to_iolist(L)]),

  % Axiom mDisYL.
%%  {true, {{PdId, mDisYL, tau, L}, Yes}};
  {true, {{PdId, mDisYL, tau, L, Yes}, Yes}};

derive_tau(L = {'or', _, _, Yes = {yes, _}}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mDisYR: ~s.", [str_pdid(PdId), m_to_iolist(L)]),

  % Axiom mDisYR.
%%  {true, {{PdId, mDisYR, tau, L}, Yes}};
  {true, {{PdId, mDisYR, tau, L, Yes}, Yes}};

derive_tau(L = {'or', _, {no, _}, M}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mDisNL: ~s.", [str_pdid(PdId), m_to_iolist(L)]),

  % Axiom mDisNL.
%%  {true, {{PdId, mDisNL, tau, L}, copy_ctx(L, M)}};
%%  {true, {{PdId, mDisNL, tau, L, copy_ctx(L, M)}, copy_ctx(L, M)}};
  {true, {{PdId, mDisNL, tau, L, copy_ns(L, copy_ctx(L, M))}, copy_ns(L, copy_ctx(L, M))}};

derive_tau(L = {'or', _, M, {no, _}}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mDisNR: ~s.", [str_pdid(PdId), m_to_iolist(L)]),

  % Axiom mDisNR.
%%  {true, {{PdId, mDisNR, tau, L}, copy_ctx(L, M)}};
%%  {true, {{PdId, mDisNR, tau, L, copy_ctx(L, M)}, copy_ctx(L, M)}};
  {true, {{PdId, mDisNR, tau, L, copy_ns(L, copy_ctx(L, M))}, copy_ns(L, copy_ctx(L, M))}};

derive_tau(L = {rec, Env, M}, PdId) ->

%%  Ns_ = [unwrap_value(get_var(Env)) | Ns],
  ?DEBUG(":: (~s) Reducing using axiom mRec: ~s.", [str_pdid(PdId), m_to_iolist(L)]),

  % The continuation of a recursive construct is encoded in terms of a function
  % that needs to be applied to unfold the monitor. Recursive monitor
  % definitions do not accept parameters.


  % Axiom mRec.
  M_ = M(),


  % Open new namespace.
%%  Env = get_env(M_),

%%  ?TRACE("Env of M_ = ~p", [Env]),

  M__ = set_env(M_, set_ns(get_env(M_), {ns, unwrap_value(get_var(Env))})),

%%  {true, {{PdId, mRec, tau, L}, copy_ctx(L, M_)}};
%%  {true, {{PdId, mRec, tau, L, copy_ctx(L, M_)}, copy_ctx(L, M_)}};
  {true, {{PdId, mRec, tau, L, copy_ctx(L, M__)}, copy_ctx(L, M__)}};

derive_tau(L = {var, Env, M}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mRec (var): ~s.", [str_pdid(PdId), m_to_iolist(L)]),

  % Recursive variables complement recursive constructs, and are used to refer
  % to recursive monitor definitions. Identically to the recursive construct,
  % the variable itself is a function reference that needs to be applied to
  % unfold the monitor. Recursive monitor definitions do not accept parameters.

  % Axiom mRec.
  M_ = M(),

  % Delete vars with NS.

  Ctx = del_bindings(get_ctx(Env), unwrap_value(get_ns(Env))),
  L_ = set_env(L, set_ctx(Env, Ctx)),


%%  ?TRACE("----- RECURSIVE CONTEXT of current monitor L: ~p", [get_ctx(get_env(L))]),
%%  ?TRACE("----- RECURSIVE CONTEXT of new reduction M_: ~p", [get_ctx(get_env(M_))]),
%%  ?TRACE("----- RECURSIVE CONTEXT of new reduction (UPDATED) M_: ~p", [get_ctx(get_env(copy_ctx(L_, M_)))]),


%%  {true, {{PdId, mRecccc, tau, L, copy_ctx(L, M_)}, copy_ctx(L, M_)}};
  {true, {{PdId, mRecccc, tau, L, copy_ctx(L_, M_)}, copy_ctx(L_, M_)}};

derive_tau(L = {Op, Env, M, N}, PdId) when Op =:= 'and'; Op =:= 'or' ->

  ?DEBUG(":: (~s) Trying to reduce using rule mTauL: ~s.", [str_pdid(PdId), m_to_iolist(L)]),
%%  case derive_tau(copy_ctx(L, M), new_pdid(PdId)) of
  case derive_tau(copy_ns(L, copy_ctx(L, M)), new_pdid(PdId)) of
    false ->
      ?DEBUG(":: (~s) Trying to reduce using rule mTauR: ~s.", [str_pdid(PdId), m_to_iolist(L)]),
%%      case derive_tau(copy_ctx(L, N), new_pdid(PdId)) of
      case derive_tau(copy_ns(L, copy_ctx(L, N)), new_pdid(PdId)) of
        false ->
          ?DEBUG(":: (~s) Unable to reduce futher using tau: ~s.", [str_pdid(PdId), m_to_iolist(L)]),
          false;
        {true, {PdN_, N_}} ->

          % Rule mTauR.
          {true, {{PdId, mTauR, tau, L, copy_ns(L, copy_ctx(L, M)), N_, {pre, PdN_}}, {Op, Env, M, N_}}}
%%          {true, {{PdId, mTauR, tau, L, copy_ns(L, copy_ctx(L, M)), N_, {pre, PdN_}}, {Op, {env, [{str, "TAUR"}]}, M, N_}}}
      end;
    {true, {PdM_, M_}} ->

      % Rule mTauL.
      {true, {{PdId, mTauL, tau, L, M_, copy_ns(L, copy_ctx(L, N)), {pre, PdM_}}, {Op, Env, M_, N}}}
%%      {true, {{PdId, mTauL, tau, L, M_, copy_ns(L, copy_ctx(L, N)), {pre, PdM_}}, {Op, {env, [{str, "TAUL"}]}, M_, N}}}
  end;

derive_tau(_, _) ->

  % The monitor cannot transition internally on tau actions.
  false.


% Proof derivation strategy for rules that transition via external actions.
derive_act(Act, M = {V, _}, PdId) when V =:= yes; V =:= no ->
  ?assertNot(Act =:= tau),
  ?DEBUG(":: (~s) Reducing using axiom mVrd: ~s.", [str_pdid(PdId), m_to_iolist(M)]),

  % Axiom mVrd.
  {{PdId, mVrd, Act, M, M}, M};

derive_act(Act, L = {act, Env, C, M}, PdId) ->
  ?assertNot(Act =:= tau),
  ?assert(C(Act)),
  ?assert(is_function(M, 1)),
%%  ?DEBUG(":: (~s) Reducing using rule mAct: ~s.", [str_pdid(PdId), get_str(Env)]),


  % Get the variable binder associated with this action.
  Binder = unwrap_value(get_var(Env)),

  % Instantiate binder with data from action and extend the variable context.
  % This is used for debugging purposes, to track the flow of data values in the
  % monitor and its continuation.

  Ns = get_ns(Env),
  L_ = set_env(L, set_ctx(Env, new_binding(get_ctx(Env), unwrap_value(Ns), Binder, Act))),


  ?DEBUG(":: (~s) Reducing using rule mAct: ~s.", [str_pdid(PdId), m_to_iolist(L_)]),


  % Axiom mAct.
  M_ = M(Act),
  ?assertNot(is_function(M_)),

%%  M__ = set_ns()

  % The environment of M cannot be updated prior to applying M to Act, since M
  % is a function. Once M is applied, the new variable binding acquired during
  % the analysis of Act can be passed down to the unwrapped monitor by updating
  % its environment.

%%  NewM = copy_ctx(MMM, M_),
%%  NewM = copy_ctx(L_, M_),


%%  {{PdId, mAct, Act, {act, NewEnv}}, NewM}; % Updated monitor env.
%%  {{PdId, mAct, Act, MMM}, NewM}; % Updated monitor env.
%%  {{PdId, mAct, Act, L_}, M_}; % Updated monitor env.
%%  {{PdId, mAct, Act, L_}, copy_ctx(L_, M_)}; % Updated monitor env.

%%  {{PdId, mAct, Act, L, copy_ctx(L_, M_)}, copy_ctx(L_, M_)}; % Updated monitor env.
  {{PdId, mAct, Act, L, copy_ns(L_, copy_ctx(L_, M_))}, copy_ns(L_, copy_ctx(L_, M_))}; % Updated monitor env.

derive_act(Act, L = {chs, _, M, N}, PdId) ->
  ?assert(is_tuple(M) andalso element(1, M) =:= act),
  ?assert(is_tuple(N) andalso element(1, N) =:= act),

  case {can_act(Act, M), can_act(Act, N)} of
    {true, false} ->
      ?DEBUG(":: (~s) Reducing using rule mChsL: ~s.", [str_pdid(PdId), m_to_iolist(L)]),

      % Rule mChsL.
      {PdM_, M_} = derive_act(Act, copy_ns(L, copy_ctx(L, M)), new_pdid(PdId)),
%%      ?TRACE("---- Updated context from premises: ~p", [get_ctx(get_env(M_))]),
%%      {{PdId, mChsL, Act, L, {pre, PdM_}}, M_};
      {{PdId, mChsL, Act, L, M_, {pre, PdM_}}, M_};
%%      {{PdId, mChsL, Act, copy_ctx(M_, L), {pre, PdM_}}, M_};
%%      {{PdId, mChsL, Act, M_, {pre, PdM_}}, M_};

    {false, true} ->
      ?DEBUG(":: (~s) Reducing using rule mChsR: ~s.", [str_pdid(PdId), m_to_iolist(L)]),

      % Rule mChsR.
      {PdN_, N_} = derive_act(Act, copy_ns(L, copy_ctx(L, N)), new_pdid(PdId)),
%%      ?TRACE("---- Updated context from premises: ~p", [get_ctx(get_env(N_))]),
%%      {{PdId, mChsR, Act, L, {pre, PdN_}}, N_}
      {{PdId, mChsR, Act, L, N_, {pre, PdN_}}, N_}
%%      {{PdId, mChsR, Act, copy_ctx(N_, L), {pre, PdN_}}, N_}
  end;

derive_act(Act, L = {Op, Env, M, N}, PdId) when Op =:= 'and'; Op =:= 'or' ->
  ?assertNot(Act =:= tau),
  ?DEBUG(":: (~s) Reducing using rule mPar: ~s.", [str_pdid(PdId), m_to_iolist(L)]),

  % Unfold respective sub-monitors. Proof derivation ID for second monitor N is
  % incremented accordingly.
  {PdM_, M_} = derive_act(Act, copy_ns(L, copy_ctx(L, M)), new_pdid(PdId)),
  {PdN_, N_} = derive_act(Act, copy_ns(L, copy_ctx(L, N)), inc_pdid(new_pdid(PdId))),


%%  Merged = merge_ctx(get_ctx(get_env(M_)), get_ctx(get_env(N_))),
%%  ?DEBUG(":: (~s) REDUCED using rule mPar: ~s.", [str_pdid(PdId), m_to_iolist(set_env(L, set_ctx(get_env(L), Merged)))]),

  % Merge context.
  ?TRACE("--> CTX M_ ~p", [get_ctx(get_env(M_))]),
  ?TRACE("--> CTX N_ ~p", [get_ctx(get_env(N_))]),
  Ctx = merge_ctx(get_ctx(get_env(M_)), get_ctx(get_env(N_))),
  ?TRACE("--> Merged context ~p", [Ctx]),
  Env_ = set_ctx(Env, Ctx),






%%  {{PdId, mPar, Act, L, M_, N_, {pre, PdM_}, {pre, PdN_}}, {Op, Env, M_, N_}}.
  {{PdId, mPar, Act, L, M_, N_, {pre, PdM_}, {pre, PdN_}}, {Op, Env_, M_, N_}}.
%%  {{PdId, mPar, Act, L, M_, N_, {pre, PdM_}, {pre, PdN_}}, {Op, Env, M_, N_}}.
%%  {{PdId, mPar, Act, L, M_, N_, {pre, PdM_}, {pre, PdN_}}, {Op, get_env(N_), M_, N_}}. % Use context of N_ ?
%%  {{PdId, mPar, Act, L, {pre, PdM_}, {pre, PdN_}}, {Op, Env, M_, N_}}.
%%  {{PdId, mPar, Act, set_env(L, Env_), {pre, PdM_}, {pre, PdN_}}, {Op, Env, M_, N_}}.


% The env should be the updated Env, always. Depends on how you decide to print
% it after all, since it's not necessary for the correct operation of monitors,
% but only for the correct stringifying of monitors.






can_act(Act, {act, _Env, C, _M}) ->
  ?assert(is_function(_M, 1)),
  C(Act).



reduce_tau(M, PdLst) ->
  ?TRACE("[ Attempting a new derivation for monitor on action 'tau' ]"),

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
  ?TRACE("[ Starting a new derivation for monitor on action '~w' ]", [Act]),

  % Analyze action.
  {PdM, M_} = derive_act(Act, M, new_pdid([])),

  % Check whether the residual monitor state can be reduced further using tau
  % transitions. This ensures that the monitor is always left in a state where
  % it is ready to analyse the next action.
  reduce_tau(M_, [PdM | PdLst]).



%%analyze([], M) ->
%%  ?TRACE("[ Analysis ready ]");

analyze_trace(Trace, M) when is_list(Trace) ->
  {PdList_, M_} = reduce_tau(M, []),
  analyze_trace(Trace, M_, PdList_).

analyze_trace([], M, PdList) ->
  {PdList, M};

analyze_trace([Act | Trace], M, PdList) ->
  {PdList_, M_} = analyze(Act, M, PdList),
  analyze_trace(Trace, M_, PdList_).

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

% Returns the event pattern.
get_pat({env, Env}) when is_list(Env) ->
  get_key(pat, Env, false).


% Returns the existing context or an empty one if none exists.
get_ctx({env, Env}) when is_list(Env) ->
  get_key(?KEY_ENV_CTX, Env, {true, []}).

set_ctx({env, Env}, {ctx, Ctx}) when is_list(Env), is_list(Ctx) ->
  {env, put_key(?KEY_ENV_CTX, Ctx, Env)}.


new_binding({ctx, Ctx}, Ns, Name, Value) when is_list(Ctx) ->
  {ctx, put_key({Ns, Name}, Value, Ctx)}.


del_bindings({ctx, []}, _) ->
  {ctx, []};

del_bindings({ctx, [{{Ns, _}, _} | Bindings]}, Ns) ->
  del_bindings({ctx, Bindings}, Ns);

del_bindings({ctx, [Binding = {{_, _}, _} | Bindings]}, Ns) ->
  {ctx, Bindings_} = del_bindings({ctx, Bindings}, Ns),
  {ctx, [Binding | Bindings_]}.



get_ns({env, Env}) ->
  get_key(ns, Env, {true, global}).

set_ns({env, Env}, {ns, Ns}) ->
  {env, put_key(ns, Ns, Env)}.


copy_ns(From, To) ->
  EnvTo = set_ns(get_env(To), get_ns(get_env(From))),
  set_env(To, EnvTo).


% From: Monitor; To: monitor.
copy_ctx(From, To) ->
  EnvTo = set_ctx(get_env(To), get_ctx(get_env(From))),
  set_env(To, EnvTo).

merge_ctx({ctx, Ctx1}, {ctx, Ctx2}) ->
  {ctx, lists:foldr(
    fun(Mapping = {Name, _}, Acc) ->
      case get_key(Name, Acc, false) of
        false ->
          [Mapping | Acc];
        {Name, _} ->
          Acc
      end
    end, Ctx2, Ctx1)}.

unwrap_value({_, Value}) ->
  Value.



% This relies on the fact that the derivation algorithm copies the context from
% one monitor continuation to the other so inherit it. But since we are printing
% a monitor that has not been reduce, we need to pass the context to the
% continuation which has not been yet unfolded.
m_to_iolist(M) ->

  % Pass variable context of monitor so that monitors containing free variables
  % are correctly stringified.
  {ctx, Ctx} = get_ctx(get_env(M)),
  m_to_iolist(M, [{Name, Value} || {{_, Name}, Value} <- Ctx]).

m_to_iolist({yes, Env = {env, _}}, _) ->
  unwrap_value(get_str(Env));
m_to_iolist({no, Env = {env, _}}, _) ->
  unwrap_value(get_str(Env));
m_to_iolist({var, Env = {env, _}, _}, _) ->
  unwrap_value(get_str(Env));
m_to_iolist({act, Env = {env, _}, _, M}, Ctx) ->

  % The continuation of an action is a function. In order to stringify the rest
  % of the monitor, apply the function to unfold it. Action functions accept a
  % single parameter.
  M_ = M(undef),
  [format_ph(unwrap_value(get_str(Env)), Ctx), $., m_to_iolist(M_, Ctx)];
m_to_iolist({chs, Env = {env, _}, M, N}, Ctx) ->
  [$(, m_to_iolist(M, Ctx), $ , unwrap_value(get_str(Env)), $ , m_to_iolist(N, Ctx), $)];
m_to_iolist({'or', Env = {env, _}, M, N}, Ctx) ->
  [m_to_iolist(M, Ctx), $ , unwrap_value(get_str(Env)), $ , m_to_iolist(N, Ctx)];
m_to_iolist({'and', Env = {env, _}, M, N}, Ctx) ->
  [m_to_iolist(M, Ctx), $ , unwrap_value(get_str(Env)), $ , m_to_iolist(N, Ctx)];
m_to_iolist({rec, Env = {env, _}, M}, Ctx) ->

  % The continuation of a recursive construct is encoded in terms of a function
  % that needs to be applied to unfold the monitor before stringifying it.
  % Recursive monitor definitions do not accept parameters.
  [unwrap_value(get_str(Env)), m_to_iolist(M(), Ctx)].


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


format_pdlist(PdList) ->
  lists:foldl(
    fun(Pd, {I, IoList}) ->
      {I - 1, [[io_lib:format("~nDerivation ~w:~n", [I]), fmt_pd(Pd)] | IoList]}
    end,
    {length(PdList), []}, PdList
  ).

show_pdlist(PdList) ->
  {_, IoList} = format_pdlist(PdList),
  io:format("~s~n", [IoList]).



fmt_pd({PdId, Rule, Act, M, M_}) ->
  Indent = length(PdId) + length(?PD_SEP),
  io_lib:format("~*s [~s, axiom ~s] ~s~n~*s-(~w)->~n~*s~s~n",
    [Indent, ?PD_SEP, str_pdid(PdId), Rule, format_m(M), Indent + 1, "", Act, Indent + 1, "", format_m(M_)]
%%    [Indent, ?PD_SEP, str_pdid(PdId), Rule, format_m2(M), Indent + 1, "", Act, Indent + 1, "", format_m2(M_)]
  );




fmt_pd({PdId, Rule, Act, M, M_, {pre, PdM}}) -> % mChs
  PdMFmt = fmt_pd(PdM),
  Indent = length(PdId) + length(?PD_SEP),
  [io_lib:format("~*s [~s, rule ~s] ~s~n~*s-(~w)->~n~*s~s~n",
    [Indent, ?PD_SEP, str_pdid(PdId), Rule, format_m(M), Indent + 1, "", Act, Indent + 1, "", format_m(M_)])
%%    [Indent, ?PD_SEP, str_pdid(PdId), Rule, format_m2(M), Indent + 1, "", Act, Indent + 1, "", format_m2(M_)])
    | PdMFmt
  ];

fmt_pd({PdId, Rule, Act, M, M_, N_, {pre, PdM}}) -> % mTauL and mTauR
  PdMFmt = fmt_pd(PdM),
  Indent = length(PdId) + length(?PD_SEP),
  [io_lib:format("~*s [~s, rule ~s] ~s~n~*s-(~w)->~n~*s ~s ~s ~s~n",
    [length(PdId) + 1, "-", str_pdid(PdId), Rule, format_m(M), Indent + 1, "", Act, Indent + 1, "", format_m(M_), unwrap_value(get_str(get_env(M))), format_m(N_)])
%%    [length(PdId) + 1, "-", str_pdid(PdId), Rule, format_m2(M), Indent + 1, "", Act, Indent + 1, "", format_m2(M_), unwrap_value(get_str(get_env(M))), format_m2(N_)])
    | PdMFmt
  ];


fmt_pd({PdId, Rule, Act, M, M_, N_, {pre, PdM}, {pre, PdN}}) ->
  {PdMFmt, PdNFmt} = {fmt_pd(PdM), fmt_pd(PdN)},
  Indent = length(PdId) + length(?PD_SEP),
  [
    [
      io_lib:format("~*s [~s, rule ~s] ~s~n~*s-(~w)->~n~*s~s ~s ~s~n",
        [length(PdId) + 1, "-", str_pdid(PdId), Rule, format_m(M), Indent + 1, "", Act, Indent + 1, "", format_m(M_), unwrap_value(get_str(get_env(M))), format_m(N_)])
%%        [length(PdId) + 1, "-", str_pdid(PdId), Rule, format_m2(M), Indent + 1, "", Act, Indent + 1, "", format_m2(M_), unwrap_value(get_str(get_env(M))), format_m2(N_)])
      | PdMFmt
    ]
    | PdNFmt
  ].

% For this we do not need to pass the context, since all the variable
% information is contained in the proof derivation.


format_m(M) ->
  {ctx, Ctx} = get_ctx(get_env(M)),
  Vars = [{Name, Value} || {{_, Name}, Value} <- Ctx],
  lists:flatten(io_lib:format("~s sub([ ~s])", [format_m(M, Vars),
    [io_lib:format("~s=~w ", [Name, Value]) || {Name, Value} <- Vars]])).


format_m({yes, Env = {env, _}}, _) ->
  unwrap_value(get_str(Env));
format_m({no, Env = {env, _}}, _) ->
  unwrap_value(get_str(Env));
format_m({var, Env = {env, _}, _}, _) ->
  unwrap_value(get_str(Env));
format_m({act, Env = {env, _}, _, M}, Ctx) ->

  % Unfold continuation monitor body for act using dummy data. This data will
  % not interfere with constraints since there are no constraints associated
  % with the continuation body, but only with the action guard test.
  M_ = M(unwrap_value(get_pat(Env))),

%%  [format_ph(unwrap_value(get_str(Env)), Ctx), $., format_m(M_, Ctx)];
%%  [format_ph(re:replace(unwrap_value(get_str(Env)), " when ", ","), Ctx), $., format_m(M_, Ctx)];
  [re:replace(unwrap_value(get_str(Env)), " when ", ","), $., format_m(M_, Ctx)];
format_m({chs, Env = {env, _}, M, N}, Ctx) ->
  [$(, format_m(M, Ctx), $ , unwrap_value(get_str(Env)), $ , format_m(N, Ctx), $)];
format_m({'or', Env = {env, _}, M, N}, Ctx) ->
  [format_m(M, Ctx), $ , unwrap_value(get_str(Env)), $ , format_m(N, Ctx)];
format_m({'and', Env = {env, _}, M, N}, Ctx) ->
  [format_m(M, Ctx), $ , unwrap_value(get_str(Env)), $ , format_m(N, Ctx)];
format_m({rec, Env = {env, _}, M}, Ctx) ->
  [unwrap_value(get_str(Env)), format_m(M(), Ctx)].




%%filter_ctx({ctx, Ctx}) ->
%%  [{Name, Value} || {{_, Name}, Value} <- Ctx].
%%
%%str_ctx({ctx, Ctx}) ->
%%  Vars = filter_ctx({ctx, Ctx}),
%%  lists:flatten(io_lib:format(" sub([ ~s])   ", [[io_lib:format("~s=~w ", [Name, Value]) || {Name, Value} <- Vars]])).

%%format_m2({yes, Env = {env, _}}) ->
%%%%  [unwrap_value(get_str(Env)), str_ctx(get_ctx(Env))];
%%  [unwrap_value(get_str(Env))];
%%format_m2({no, Env = {env, _}}) ->
%%%%  [unwrap_value(get_str(Env)), str_ctx(get_ctx(Env))];
%%  [unwrap_value(get_str(Env))];
%%format_m2({var, Env = {env, _}, _}) ->
%%%%  [unwrap_value(get_str(Env)), str_ctx(get_ctx(Env))];
%%  [unwrap_value(get_str(Env))];
%%format_m2({act, Env = {env, _}, _, M}) ->
%%  M_ = M(undef),
%%  [format_ph(re:replace(unwrap_value(get_str(Env)), " when ", ","), filter_ctx(get_ctx(Env))), str_ctx(get_ctx(Env)), $., format_m2(M_)];
%%%%  ?TRACE("IN Act. M_ = ~p", [M_]),
%%%%  [format_ph(re:replace(unwrap_value(get_str(Env)), " when ", ","), filter_ctx(get_ctx(Env))), $., format_m2(M_)];
%%format_m2({chs, Env = {env, _}, M, N}) ->
%%%%  [$(, format_m2(M), $ , unwrap_value(get_str(Env)), $ , format_m2(N), $), str_ctx(get_ctx(Env))];
%%%%  ?TRACE("IN CHS. M = ~p ADN N = ~p", [M, N]),
%%  [$(, format_m2(M), $ , unwrap_value(get_str(Env)), $ , format_m2(N), $)];
%%format_m2({'or', Env = {env, _}, M, N}) ->
%%%%  [format_m2(M), $ , unwrap_value(get_str(Env)), $ , format_m2(N), str_ctx(get_ctx(Env))];
%%  [format_m2(M), $ , unwrap_value(get_str(Env)), $ , format_m2(N)];
%%format_m2({'and', Env = {env, _}, M, N}) ->
%%%%  [format_m2(M), $ , unwrap_value(get_str(Env)), $ , format_m2(N), str_ctx(get_ctx(Env))];
%%  [format_m2(M), $ , unwrap_value(get_str(Env)), $ , format_m2(N)];
%%format_m2({rec, Env = {env, _}, M}) ->
%%%%  [unwrap_value(get_str(Env)), format_m2(M()), str_ctx(get_ctx(Env))].
%%  [unwrap_value(get_str(Env)), format_m2(M())].

