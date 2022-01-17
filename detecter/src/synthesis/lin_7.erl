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
-define(KEY_ENV_RVAR, var).
-define(KEY_ENV_DVARS, vars).
-define(KEY_ENV_PDID, pdid).


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
            {env, [{str, "{:A} when true"}, {vars, [{'A', undef}]}]},
            fun(A) -> true; (_) -> false end,
            fun(A) ->
              {'and',
                {env, [{str, "and"}]},
                {chs,
                  {env, [{str, "+"}]},
                  {act,
                    {env, [{str, "{:B} when {:A} =:= {:B}"}, {vars, [{'A', undef}, {'B', undef}]}]},
                    fun(B) when A =:= B -> true; (_) -> false end,
                    fun(B) ->
                      {no, {env, [{str, "no"}]}}
                    end
                  },
                  {act,
                    {env, [{str, "{:B} when not({:A} =:= {:B})"}, {vars, [{'A', undef}, {'B', undef}]}]},
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
            {env, [{str, "{:A} when not(true)"}, {vars, [{'A', undef}]}]},
            fun(A) -> false; (_) -> true end,
            fun(A) ->
              {yes, {env, [{str, "yes"}]}}
            end
          }
        }
      end
    }
  }.

%%m_str() ->
%%  {ok,
%%    {rec,
%%      fun X() ->
%%        {var, }
%%      end}
%%    }.


% The ID can be saved in the Env probably. The env is used or verbosing and IDs!

% Proof derivation strategy for rules that transition via the internal action tau .
derive_tau({'and', {env, Env}, {yes, _}, M}, PdId) ->

  ?DEBUG(":: (~s) Reducing using axiom mConYL: ~s.", [str_pdid(PdId), get_str(Env)]),

  % Axiom mConYL.
  {true, {{PdId, mConYL, tau, Env}, M}};

derive_tau({'and', {env, Env}, M, {yes, _}}, PdId) ->

  ?DEBUG(":: (~s) Reducing using axiom mConYR: ~s.", [str_pdid(PdId), get_str(Env)]),

  % Axiom mConYR.
  {true, {{PdId, mConYR, tau, Env}, M}};

derive_tau({'and', {env, Env}, No = {no, _}, _}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mConNL: ~s.", [str_pdid(PdId), get_str(Env)]),

  % Axiom mConNL.
  {true, {{PdId, mConNL, tau, Env}, No}};

derive_tau({'and', {env, Env}, _, No = {no, _}}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mConNR: ~s.", [str_pdid(PdId), get_str(Env)]),

  % Axiom mConNR.
  {true, {{PdId, mConNR, tau, Env}, No}};

derive_tau({'or', {env, Env}, Yes = {yes, _}, _}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mDisYL: ~s.", [str_pdid(PdId), get_str(Env)]),

  % Axiom mDisYL.
  {true, {{PdId, mDisYL, tau, Env}, Yes}};

derive_tau({'or', {env, Env}, _, Yes = {yes, _}}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mDisYR: ~s.", [str_pdid(PdId), get_str(Env)]),

  % Axiom mDisYR.
  {true, {{PdId, mDisYR, tau, Env}, Yes}};

derive_tau({'or', {env, Env}, {no, _}, M}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mDisNL: ~s.", [str_pdid(PdId), get_str(Env)]),

  % Axiom mDisNL.
  {true, {{PdId, mDisNL, tau, Env}, M}};

derive_tau({'or', {env, Env}, M, {no, _}}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mDisNR: ~s.", [str_pdid(PdId), get_str(Env)]),

  % Axiom mDisNR.
  {true, {{PdId, mDisNR, tau, Env}, M}};

derive_tau({rec, {env, Env}, M}, PdId) ->
  ?DEBUG(":: (~s) Reducing using axiom mRec: ~s.", [str_pdid(PdId), get_str(Env)]),

  % Axiom mRec.
  M_ = M(),
  {true, {{PdId, mRec, tau, Env}, M_}};

derive_tau({Op, {env, Env}, M, N}, PdId) when Op =:= 'and'; Op =:= 'or' ->

  ?DEBUG(":: (~s) Trying to reduce using rule mTauL: ~s.", [str_pdid(PdId), get_str(Env)]),
  case derive_tau(M, new_pdid(PdId)) of
    false ->
      ?DEBUG(":: (~s) Trying to reduce using rule mTauR: ~s.", [str_pdid(PdId), get_str(Env)]),
      case derive_tau(N, new_pdid(PdId)) of
        false ->
          false;
        {true, {PdN, N_}} ->
          {true, {{PdId, mTauR, tau, Env, {pd, PdN}}, {Op, element(2, M) ++ " " ++ atom_to_list(Op) ++ " " ++ element(2, N_), M, N_}}}
      end;
    {true, {PdM, M_}} ->
      {true, {{PdId, mTauL, tau, Env, {pd, PdM}}, {Op, element(2, M_) ++ " " ++ atom_to_list(Op) ++ " " ++ element(2, N), M_, N}}}
  end;

derive_tau(_, _) ->
  false.

% TODO: Change the code to make use of the env system I'm trying to use now


% Proof derivation strategy for rules that transition via external actions.
derive_act(Act, V_ = {V, {env, Env}}, PdId) when V =:= yes; V =:= no ->
  ?assertNot(Act =:= tau),
  ?DEBUG(":: (~s) Reducing using axiom mVrd: ~s.", [str_pdid(PdId), get_str(Env)]),

  % Axiom mVrd.
  {{PdId, mVrd, Act, Env}, V_};

derive_act(Act, {act, {env, Env}, C, M}, PdId) ->
  ?assertNot(Act =:= tau),
  ?assert(C(Act)),
  ?assert(is_function(M, 1)),
  ?DEBUG(":: (~s) Reducing using rule mAct: ~s.", [str_pdid(PdId), get_str(Env)]),

  % Axiom mAct.
  M_ = M(Act),
%%  {{Id, mAct, Act, _S}, M_};
  {{PdId, mAct, Act, Env}, M_};

derive_act(Act, {chs, {env, Env}, M, N}, PdId) ->
  ?assert(is_tuple(M) andalso element(1, M) =:= act),
  ?assert(is_tuple(N) andalso element(1, N) =:= act),

  case {can_act(Act, M), can_act(Act, N)} of
    {true, false} ->
      ?DEBUG(":: (~s) Reducing using rule mChsL: ~s.", [str_pdid(PdId), get_str(Env)]),

      % Rule mChsL.
      {PdM, M_} = derive_act(Act, M, new_pdid(PdId)),
      {{PdId, mChsL, Act, Env, {pd, PdM}}, M_};

    {false, true} ->
      ?DEBUG(":: (~s) Reducing using rule mChsR: ~s.", [str_pdid(PdId), get_str(Env)]),

      % Rule mChsR.
      {PdN, N_} = derive_act(Act, N, new_pdid(PdId)),
      {{PdId, mChsR, Act, Env, {pd, PdN}}, N_}
  end;



derive_act(Act, {Op, {env, Env}, M, N}, PdId) when Op =:= 'and'; Op =:= 'or' ->
  ?assertNot(Act =:= tau),
  ?DEBUG(":: (~s) Reducing using rule mPar: ~s.", [str_pdid(PdId), get_str(Env)]),

  {{PdM, M_}, {PdN, N_}} = {derive_act(Act, M, new_pdid(PdId)), derive_act(Act, N, inc_pdid(new_pdid(PdId)))},
  {{PdId, mPar, Act, Env, {pd, PdM}, {pd, PdN}}, {Op, element(2, M_) ++ " " ++ atom_to_list(Op) ++ " " ++ element(2, N_), M_, N_}}.


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
get_key(List, Key) when is_list(List) ->
  case lists:keyfind(Key, 1, List) of
    false ->
      false;
    {Key, Val} ->
      Val
  end.

put_key(List, Key, Value) when is_list(List) ->
  lists:keystore(Key, 1, List, {Key, Value}).

% Monitor environment related.
%%{env, [{str, "{:A} when true"}, {vars, [{'A', undef}]}]},
%%get_env(Node) when is_tuple(Node), tuple_size(Node) >= 2 ->
%%  {?KEY_ENV, Env} = element(2, Node),
%%  Env.

put_dvar(Env, Name, Value) when is_list(Env) ->
  case get_key(Env, ?KEY_ENV_DVARS) of
    false ->
      false;
    Vars ->
      put_key(Vars, Name, Value)
  end.

get_dvar(Env, Name) when is_list(Env) ->
  case get_key(Env, ?KEY_ENV_DVARS) of
    false ->
      false;
    Vars ->
      get_key(Vars, Name)
  end.

get_str(Env) ->
  get_key(Env, ?KEY_ENV_STR).

get_rvar(Env) ->
  get_key(Env, ?KEY_ENV_RVAR).

get_vars(Env) ->
  get_key(Env, ?KEY_ENV_DVARS).

put_id(Env, Value) ->
  put_key(Env, ?KEY_ENV_PDID, Value).






str_mon({yes, {env, Env}}) ->
  ?TRACE("Visting yes."),
  get_str(Env);
str_mon({no, {env, Env}}) ->
  ?TRACE("Visiting no"),
  get_str(Env);
str_mon({var, {env, Env}, _}) ->
  ?TRACE("Visiting rec var"),
  get_str(Env);
str_mon({act, {env, Env}, _, M}) ->
  ?TRACE("Visiting act"),
  format_ph(get_str(Env), get_vars(Env)) ++ "." ++ str_mon(M(ok));

str_mon({chs, {env, Env}, M, N}) ->
  ?TRACE("Visiting chs"),
  "(" ++ str_mon(M) ++ " " ++ get_str(Env) ++ " " ++ str_mon(N) ++ ")";
str_mon({'or', {env, Env}, M, N}) ->
  ?TRACE("Visiting or"),
  str_mon(M) ++ " " ++ get_str(Env) ++ " " ++ str_mon(N);
str_mon({'and', {env, Env}, M, N}) ->
  ?TRACE("Visiting and"),
  str_mon(M) ++ " " ++ get_str(Env) ++ " " ++ str_mon(N);
str_mon({rec, {env, Env}, M}) ->
  ?TRACE("Visiting rec on ~p", [get_rvar(Env)]),
  get_str(Env) ++ "(" ++ str_mon(M()) ++ ")".








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

