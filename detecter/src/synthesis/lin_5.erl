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


% P1: [a,true][b,true]ff and [a,true][b,true]ff and [a,true][b,true]ff
m1() ->
  {ok,
    {'and',
      {'and',
        {chs,
          fun(A) ->
            ?TRACE("M1: Analyzing a(~p).", [A]),
            {chs,
              fun(B) ->
                ?TRACE("M1: Analyzing b(~p).", [B]),
                no;
                (_) ->
                  yes
              end};
            (_) ->
              yes
          end},
        {chs,
          fun(A) ->
            ?TRACE("M2: Analyzing a(~p).", [A]),
            {chs,
              fun(B) ->
                ?TRACE("M2: Analyzing b(~p).", [B]),
                no;
                (_) ->
                  yes
              end};
            (_) ->
              yes
          end}
      },
      {chs,
        fun(A) ->
          ?TRACE("M3: Analyzing a(~p).", [A]),
          {chs,
            fun(B) ->
              ?TRACE("M3: Analyzing b(~p).", [B]),
              no;
              (_) ->
                yes
            end};
          (_) ->
            yes
        end}
    }
  }.

% P2:
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
m2() ->
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


% Monitor for the property that all actions are unique.
% max(X.[a,true](max(Y.[b,a=:=b]ff and [b,a=/=b]Y) and X))
m3() ->
  {ok,
    {rec,
      fun X() -> % max(X.
        {chs,
          fun(A) -> % [a,true]
            {'and',
              {rec,
                fun Y() -> % max(Y.
                  {'and',
                    {chs,
                      fun(B) when A =:= B -> % [b,a=:=b]
                        no; % ff
                        (_) -> % [b,a=/=b]
                          yes % yes
                      end},
                    {chs,
                      fun(B) when A =/= B -> % [b,a=/=b]
                        {rec, Y}; % Y
                        (_) -> % [b,a=:=b]
                          yes % yes
                      end}
                  }
                end}, % end max(Y)
              {rec, X} % X
            };
            (_) -> % [a,false]
              yes % yes
          end}
      end} % end max(X)
  }.

act_example() -> {act,
  fun(X) -> ok end, % action + constraint
  p,
  n
}.

% I know that when I have a choice, the next thing will always be an action
% that needs to be evaluated: the synthesis guarantees this. So one way to
% handle this would be to have the {chs, M, N}, and then have an
% {act, Cond, Pos, Neg} thing. Then, I would have a function con_sat() which
% is a predicate that applies the action in the choice rule mChs to the
% sub-derivation which I know will surely be an act, and return true or false
% accordingly.

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

% TODO: Ask whether this is needed, or whether we modify mPar.
rule(tau, R = {'and', M, yes}) ->
  ?DEBUG(":: Applying axiom mConY on monitor ~p.", [R]),

  % Axiom mConY.
  {mConY, M};

rule(tau, R = {'and', no, _}) ->
  ?DEBUG(":: Applying axiom mConN on monitor ~p.", [R]),

  % Axiom mConN.
  {mConN, no};

% TODO: Ask whether this is needed, or whether we modify mPar.
rule(tau, R = {'and', _, no}) ->
  ?DEBUG(":: Applying axiom mConN on monitor ~p.", [R]),

  % Axiom mConN.
  {mConN, no};

rule(tau, R = {'or', yes, _}) ->
  ?DEBUG(":: Applying axiom mDisY on monitor ~p.", [R]),

  % Axiom mDisY.
  {mDisY, yes};

% TODO: Ask whether this is needed, or whether we modify mPar.
rule(tau, R = {'or', _, yes}) ->
  ?DEBUG(":: Applying axiom mDisY on monitor ~p.", [R]),

  % Axiom mDisY.
  {mDisY, yes};

rule(tau, R = {'or', no, M}) ->
  ?DEBUG(":: Applying axiom mDisN on monitor ~p.", [R]),

  % Axiom mDisN.
  {mDisN, M};

% TODO: Ask whether this is needed, or whether we modify mPar.
rule(tau, R = {'or', M, no}) ->
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

% Rules.

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

rule(Act, R = {chs, M}) ->
  ?DEBUG(":: Applying rule mChs on monitor ~p", [R]),

  % Rule mChsL and mChsR.
  {Rule, M_} = rule(Act, M),
  {{mChs, Rule}, M_};

rule(Act, R = {Op, M, N}) ->
  ?DEBUG(":: Applying rule mPar on monitor ~p", [R]),

  % Rule mPar.
  {Rule1, M_} = rule(Act, M),
  {Rule2, N_} = rule(Act, N),
  {{mPar, Rule1, Rule2}, {Op, M_, N_}}.









can_tau({'and', V, _}) when V =:= yes; V =:= no ->
  true;
can_tau({'and', _, V}) when V =:= yes; V =:= no ->
  true;
can_tau({'or', V, _}) when V =:= yes; V =:= no ->
  true;
can_tau({'or', _, V}) when V =:= yes; V =:= no ->
  true;
%%can_tau(M = {Op, {_, _, _}, _}) when Op =:= 'and'; Op =:= 'or' ->
%%  true;
%%can_tau(M = {Op, _, {_, _, _}}) when Op =:= 'and'; Op =:= 'or' ->
%%  true;
can_tau({'and', {rec, _}, _}) ->
  true; % Can transition via mTauL.
can_tau({'and', _, {rec, _}}) ->
  true; % Can transition via mTauR.
can_tau({'or', {rec, _}, _}) ->
  true; % Can transition via mTauL.
can_tau({'or', _, {rec, _}}) ->
  true; % Can transition via mTauR.
can_tau({rec, _}) ->
  true; % Can transition via mRec.
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

  ?TRACE("Reducing monitor on taus."),

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

%%% Properties to test.

% P1: [a,true][b,true]ff and [a,true][b,true]ff and [a,true][b,true]ff
%
% Aim: To test that when the monitor ends up with {'and', {'and', no, no}, no}
% we are able to tau to a no, and then tau to a no again. This, I found only
% possible to do using the symmetric case of mConNR (which was not in the
% original axioms of the semantics in POPL). A different alternative would be
% to keep these axioms as are, but then change the mPar rule to transition over
% \mu's rather than \alpha's.
%
% The resulting monitor from this property should be:
% a,true.(b,true.ff + b,false.yes) + a,false.yes and a,true.(b,true.ff + b,false.yes) + a,false.yes and a,true.(b,true.ff + b,false.yes) + a,false.yes
%
% which can transition to ((no and no) and no) using two successive derivations
% each of which consists of two inductive applications of mPar.

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

% P3: All actions are unique.
%
% max(X.[a,true](max(Y.[b,a=:=b]ff and [b,a=/=b]Y) and X))
%
% Aim: To test the general behaviour of more than one recursive operator, and in
% particular, that data binding works correctly. The value of variable A should
% become re-instantiated when the recursive variable X is unfolded, but its
% value should persist in the inner recursion max(Y...). With each unfolding of
% X, we get a tree that expands on the left, wherein each I'th recursion
% unfolding, a new of A is obtained, corresponding to the I'th event from the
% trace. A similar reasoning applies to variable B: it should become
% instantiated with a new value when the recursive variable Y is unfolded. With
% each unfolding of Y, a new value of B is obtained, ranging over the actions
% starting from the position of the I'th event in the trace corresponding to
% the variable A, plus 1.
%
% The resulting monitor from this property should be:
% rec(x.a,true.(rec(y.(b,a=:=b.no + b,a=/=b.yes) and (b,a=/=b.y + b,a=:=b.yes)) and x) + a,false.yes)
%
% which can transition to no given the trace 1.2.1.


% PX: Unguarded variables.
