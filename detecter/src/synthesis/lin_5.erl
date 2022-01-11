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
-module(lin_5).
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


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

% TODO: Consider also extracting out the CHS rules. I think this is the only bit
% TODO: that remains so that we can fully separate the monitor syntax from the
% TODO: semantics.

% TODO: Inside the monitor string description, we need to add the values that
% No two adjacent actions are this same. This has one recursive variable which
% is not preceded by a box, but it is guarded nevertheless. The formula is as
% follows:
% max(X. [a, true]([b, a =:= b]ff and X)).
% The corresponding monitor is hand coded, and takes out the recursion
% application, so that functions have to be unfolded externally be the rule
% mRec.
%
% I have also changed the formatting of tuples for conjunctions and disjunctions
% to not use the extra nested tuple: after all, a conjunctions/disjunction is
% a binary operator. All operators are represented in prefix notation for, so
% that the operator name doubles as an Erlang tag.
m1() ->
  {ok,
    begin
      ?TRACE("Monitor: rec x.(a,true((b,a=:=b.ff + b,a=/=b.yes) and x) + a,false.yes)"),
      {rec,
        fun X() -> % max (X.
          ?TRACE("rec x.(a,true((b,a=:=b.ff + b,a=/=b.yes) and x) + a,false.yes)"),
          fun(A) -> % [a, true]
            ?TRACE("a,true((b,~p=:=b.ff + b,~p=/=b.yes) and x", [A, A]),
            begin
              ?TRACE("(b,~p=:=b.ff + b,~p=/=b.yes) and x", [A, A]),
              {'and',
                fun(B) when A =:= B -> % [b, a =:= b]
                  ?TRACE("b,~p=:=~p.ff", [A, B]),
                  no; % ff
                  (_B) -> % [b, a =/= b]
                    ?TRACE("b,~p=/=~p.yes", [A, _B]),
                    yes
                end,
                {rec, X} % X
              }
            end;
            (_A) -> % [a, false]
              ?TRACE("a,false.yes"),
              yes
          end
        end} % )
    end
  }.

m2() ->
  {ok,
    {'and',
      {'and',
        fun(A) ->
          ?TRACE("M1: Analyzing a(~p).", [A]),
          fun(B) ->
            ?TRACE("M1: Analyzing b(~p).", [A]),
            no;
            (_) ->
              yes
          end;
          (_) ->
            yes
        end,
        fun(A) ->
          ?TRACE("M2: Analyzing a(~p).", [A]),
          fun(B) ->
            ?TRACE("M2: Analyzing b(~p).", [A]),
            no;
            (_) ->
              yes
          end;
          (_) ->
            yes
        end},
      fun(A) ->
        ?TRACE("M3: Analyzing a(~p).", [A]),
        fun(B) ->
          ?TRACE("M3: Analyzing b(~p).", [A]),
          no;
          (_) ->
            yes
        end;
        (_) ->
          yes
      end
    }
  }.


%%% Axioms.
%%rule(_, R = {'and', yes, M}) ->
%%  ?DEBUG(":: Applying axiom mConY on monitor ~p.", [R]),
%%
%%  % Axiom mConY. Action not consumed.
%%  {tau, M};
%%
%%rule(_, R = {'and', no, _}) ->
%%  ?DEBUG(":: Applying axiom mConN on monitor ~p.", [R]),
%%
%%  % Axiom mConN. Action not consumed.
%%  {tau, no};
%%
%%rule(_, R = {'or', yes, _}) ->
%%  ?DEBUG(":: Applying axiom mDisY on monitor ~p.", [R]),
%%
%%  % Axiom mDisY. Action not consumed.
%%  {tau, yes};
%%
%%rule(_, R = {'or', no, M}) ->
%%  ?DEBUG(":: Applying axiom mDisN on monitor ~p.", [R]),
%%
%%  % Axiom mDisN. Action not consumed.
%%  {tau, M};
%%
%%rule(_, R = {rec, M}) ->
%%  ?DEBUG(":: Applying axiom mRec on monitor ~p.", [R]),
%%
%%  % Axiom mRec. Action not consumed.
%%  {tau, M()};
%%
%%rule(_, V) when V =:= yes; V =:= no ->
%%  ?DEBUG(":: Applying axiom mVrd on verdict ~p.", [V]),
%%
%%  % Axiom mVrd. Action not consumed.
%%  {tau, V};
%%
%%% Rules.
%%
%%
%%% mTauL and mTauR for parallel conjunction and disjunction.
%%
%%rule(Act, R = {Op, M = {Op2, _, _}, N}) ->
%%  ?DEBUG(":: Applying rule mTauL (~p) on monitor ~p to reduce ~p.", [Op2, R, M]),
%%
%%  % Rule mTauL applied on M. M is a conjunction of two monitors and can be
%%  % reduced. Action not consumed.
%%  {tau, M_} = rule(Act, M),
%%  {tau, {Op, M_, N}};
%%
%%rule(Act, R = {Op, M, N = {Op2, _, _}}) ->
%%  ?DEBUG(":: Applying rule mTauR (~p) on monitor ~p to reduce ~p.", [Op2, R, N]),
%%
%%  % Rule mTauR applied on N. N is a conjunction of two monitors and can be
%%  % reduced. Action not consumed.
%%  {tau, N_} = rule(Act, N),
%%  {tau, {Op, M, N_}};
%%
%%% mTauL and mTauR for recursion.
%%rule(Act, R = {Op, M = {rec, _}, N}) ->
%%  ?DEBUG(":: Applying rule mTauL (rec) on monitor ~p to reduce ~p.", [R, M]),
%%
%%  % Rule mTauL applied on M. M is a recursion and can be reduced. Action not
%%  % consumed.
%%  {tau, M_} = rule(Act, M), % Must be reduced by an axiom, because that is the
%%  % only rule that we can use.
%%  {tau, {Op, M_, N}};
%%
%%rule(Act, R = {Op, M, N = {rec, _}}) ->
%%  ?DEBUG(":: Applying rule mTauR (rec) on monitor ~p to reduce ~p.", [R, N]),
%%
%%  {tau, N_} = rule(Act, N),
%%  {tau, {Op, M, N_}};
%%
%%
%%
%%rule(Act, R = {Op, M, N}) ->
%%  ?DEBUG(":: Applying rule mPar on monitor ~p", [R]),
%%
%%  % Rule mPar. Action consumed.
%%  {act, {Op, rule(Act, M), rule(Act, N)}};
%%%%  {Op, rule(Act, M), rule(Act, N)};
%%
%%rule(Act, M) when is_function(M, 1) ->
%%  ?DEBUG(":: Applying rule mAct on monitor ~p.", [M]),
%%
%%  % Rule mAct. Action consumed.
%%  {act, M(Act)}.
%%  M(Act).


%%% New ----

% Return value: {Derivation rules used as a tree, Monitor State}.

% Input to rule function:
% 1. Action to transition with
% 2. Monitor.
% Output: new monitor state and derivation tree.
% Each application of a rule will ALWAYS lead me to a base case, and therefore,
% to the end of the derivation.


% Add the symmetric cases of and with verdicts yes and no.
% Axioms.
rule(tau, R = {'and', yes, M}) ->
  ?DEBUG(":: Applying axiom mConY on monitor ~p.", [R]),

  % Axiom mConY.
  {mConY, M};

rule(tau, R = {'and', no, _}) ->
  ?DEBUG(":: Applying axiom mConN on monitor ~p.", [R]),

  % Axiom mConN.
  {mConN, no};

rule(tau, R = {'or', yes, _}) ->
  ?DEBUG(":: Applying axiom mDisY on monitor ~p.", [R]),

  % Axiom mDisY.
  {mDisY, yes};

rule(tau, R = {'or', no, M}) ->
  ?DEBUG(":: Applying axiom mDisN on monitor ~p.", [R]),

  % Axiom mDisN.
  {mDisN, M};

rule(tau, R = {rec, M}) ->
  ?DEBUG(":: Applying axiom mRec on monitor ~p.", [R]),

  % Axiom mRec.
  {mRec, M()};

rule(_, V) when V =:= yes; V =:= no ->
  ?DEBUG(":: Applying axiom mVrd on verdict ~p.", [V]),

  % Axiom mVrd.
  {mVrd, V};

rule(Act, M) when is_function(M, 1) ->
  ?DEBUG(":: Applying rule mAct on monitor ~p.", [M]),

  % Axiom mAct.
  {mAct, M(Act)};

% TODO: Must be tau.
%%rule(Act = tau, R = {Op, M = {Op2, _, _}, N}) ->
%%  ?DEBUG(":: Applying rule mTauL (~p) on monitor ~p to reduce ~p.", [Op2, R, M]),
%%
%%  % Rule mTauL.
%%  {Rule, M_} = rule(Act, M),
%%  {{mTauL, Rule}, {Op, M_, N}};
%%
%%rule(Act = tau, R = {Op, M, N = {Op2, _, _}}) ->
%%  ?DEBUG(":: Applying rule mTauR (~p) on monitor ~p to reduce ~p.", [Op2, R, N]),
%%
%%  % Rule mTauR.
%%  {Rule, N_} = rule(Act, N),
%%  {{mTauR, Rule}, {Op, M, N_}};

rule(Act = tau, R = {Op, M = {rec, _}, N}) ->
  ?DEBUG(":: Applying rule mTauL (rec) on monitor ~p to reduce ~p.", [R, M]),

  % Rule mTauL.
  {Rule, M_} = rule(Act, M),
  {{mTauL, Rule}, {Op, M_, N}};

rule(Act = tau, R = {Op, M, N = {rec, _}}) ->
  ?DEBUG(":: Applying rule mTauR (rec) on monitor ~p to reduce ~p.", [R, N]),

  % Rule mTauR.
  {Rule, N_} = rule(Act, N),
  {{mTauR, Rule}, {Op, M, N_}};

rule(Act, R = {Op, M, N}) ->
  ?DEBUG(":: Applying rule mPar on monitor ~p", [R]),

  % Rule mPar.
  {Rule1, M_} = rule(Act, M),
  {Rule2, N_} = rule(Act, N),
  {{mPar, Rule1, Rule2}, {Op, M_, N_}}.




%%can_tau({'and', V, V_} when either of them is a verdict)


% Expand these to make them explicit.


can_tau(M = {'and', V, _}) when V =:= yes; V =:= no ->
  true;
can_tau(M = {'or', V, _}) when V =:= yes; V =:= no ->
  true;
%%can_tau(M = {Op, {_, _, _}, _}) when Op =:= 'and'; Op =:= 'or' ->
%%  true;
%%can_tau(M = {Op, _, {_, _, _}}) when Op =:= 'and'; Op =:= 'or' ->
%%  true;
can_tau(M = {Op, {rec, _}, _}) when Op =:= 'and'; Op =:= 'or' ->
  true;
can_tau(M = {Op, _, {rec, _}}) when Op =:= 'and'; Op =:= 'or' ->
  true;
can_tau(M = {rec, _}) ->
  true;
can_tau(_) ->
  false.


% Returns: {PdLst, M_}
reduce(M, PdLst) ->
  case can_tau(M) of
    true ->

      % Monitor can reduce internally via one or more tau transitions. Axioms
      % reduce the monitor by one step; rules recursively apply reductions until
      % an axiom is reached. Applying the rule results in a full proof
      % derivation.
      {Pd, M_} = rule(tau, M),

      % The resulting monitor may not necessarily be in a state that it is able
      % to analyse a concrete action yet. Try reducing further.
      reduce(M_, [Pd | PdLst]);

    false ->

      % Monitor cannot reduce further via tau transitions.
      {PdLst, M}
  end.


% M = {chs, _, _}
% M = {par, _, _}
% M = fun
analyze(Act, M, PdLst) ->

  % Assume that monitor is reduced and ready to analyse an action.
  % Assert something.

  % Analyze action.
  {Pd, M_} = rule(Act, M),

  % Reduce monitor state to one where it is ready to analyse the next action.
  reduce(M_, [Pd | PdLst]).


% TODO: Extract the "intelligence of applying the rules" from the rules to the
% TODO: new function that we built which will be called derive(Act, Mon). Then
% TODO: I suppose that the rule function can be split into axom(Mon) and
% TODO: rule(Act, Mon)? We'll see.

% TODO: Try a monitor with an unguarded variable, which should loop forever.

% TODO: Reduce the monitor until the next action needs to be consumed. We do this
% TODO: by taking into account verdict states in ands and ors.

% TODO: Test whether we need the rules for conjunctions and disjunctions where
% TODO: the verdict is on the RHS, rather than on the LHS. One example would be
% TODO:

% TODO: Improve the analyze function so that we have one rule TauL and TauR in
% TODO: rules, and then we know when to apply it in the analyse function. So the
% TODO: 'intelligence' as to what reduction rule to apply should be encoded in
% TODO: the analyze function. Might as well name it derive(Act, Mon)? Could be.

%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------
