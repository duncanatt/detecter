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
-module(lin_2).
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

% TODO: Consider also extracting out the CHS rules. I think this is the only bit
% TODO: that remains so that we can fully separate the monitor syntax from the
% TODO: semantics.

% TODO: Inside the monitor string description, we need to add the values that
% TODO: have been assigned to variables.

% Axioms.
rule(_, R = {'and', yes, M}) ->
  ?DEBUG(":: Applying axiom mConY on monitor ~p.", [R]),

  % Axiom mConY. Action not consumed.
  {tau, M};

rule(_, R = {'and', no, _}) ->
  ?DEBUG(":: Applying axiom mConN on monitor ~p.", [R]),

  % Axiom mConN. Action not consumed.
  {tau, no};

rule(_, R = {'or', yes, _}) ->
  ?DEBUG(":: Applying axiom mDisY on monitor ~p.", [R]),

  % Axiom mDisY. Action not consumed.
  {tau, yes};

rule(_, R = {'or', no, M}) ->
  ?DEBUG(":: Applying axiom mDisN on monitor ~p.", [R]),

  % Axiom mDisN. Action not consumed.
  {tau, M};

rule(_, R = {rec, M}) ->
  ?DEBUG(":: Applying axiom mRec on monitor ~p.", [R]),

  % Axiom mRec. Action not consumed.
  {tau, M()};

rule(_, V) when V =:= yes; V =:= no ->
  ?DEBUG(":: Applying axiom mVrd on verdict ~p.", [V]),

  % Axiom mVrd. Action not consumed.
  {tau, V};

% Rules.

% mTauL and mTauR for parallel conjunction and disjunction.

rule(Act, R = {Op, M = {Op2, _, _}, N}) ->
  ?DEBUG(":: Applying rule mTauL (~p) on monitor ~p to reduce ~p.", [Op2, R, M]),

  % Rule mTauL applied on M. M is a conjunction of two monitors and can be
  % reduced. Action not consumed.
  {tau, M_} = rule(Act, M),
  {tau, {Op, M_, N}};

rule(Act, R = {Op, M, N = {Op2, _, _}}) ->
  ?DEBUG(":: Applying rule mTauR (~p) on monitor ~p to reduce ~p.", [Op2, R, N]),

  % Rule mTauR applied on N. N is a conjunction of two monitors and can be
  % reduced. Action not consumed.
  {tau, N_} = rule(Act, N),
  {tau, {Op, M, N_}};

rule(Act, R = {Op, M = {rec, _}, N}) ->
  ?DEBUG(":: Applying rule mTauL (rec) on monitor ~p to reduce ~p.", [R, M]),

  % Rule mTauL applied on M. M is a recursion and can be reduced. Action not
  % consumed.
  {tau, M_} = rule(Act, M),
  {tau, {Op, M_, N}};

rule(Act, R = {Op, M, N = {rec, _}}) ->
  ?DEBUG(":: Applying rule mTauR (rec) on monitor ~p to reduce ~p.", [R, N]),

  {tau, N_} = rule(Act, N),
  {tau, {Op, M, N_}};

rule(Act, R = {Op, M, N}) when is_function(M, 1), is_function(N, 1) ->
  ?DEBUG(":: Applying rule mPar on monitor ~p", [R]),

  % Rule mPar. Action consumed.
%%  {act, {Op, rule(Act, M), rule(Act, N)}};
  {Op, rule(Act, M), rule(Act, N)};

rule(Act, M) when is_function(M, 1) ->
  ?DEBUG(":: Applying rule mAct on monitor ~p.", [M]),

  % Rule mAct. Action consumed.
%%  {act, M(Act)}.
  M(Act).


% Assumes that M is already a monitor.
analyze(Act, M) ->

  ?INFO("New input monitor: ~p.", [M]),

  Red = rule(Act, M),
  ?INFO("Reduced monitor: ~p.", [Red]),
  case Red of
    {tau, V} when V =:= yes; V =:= no ->

      % Ensures that we do not have an infinite loop on the verdict state.
      ?INFO("Monitoring verdict reached: ~p.", [V]),
      V;

    {tau, M_} ->
      % Monitor was reduced by one or more tau transitions and action was not
      % analysed. Reapply analysis to new monitor reduction.

      analyze(Act, M_);

    Mon = {_, V, _} when V =:= yes; V =:= no ->
      io:format(">> Reached verdict state and advancing automatically~n"),
      analyze(Act, Mon);

    M_ ->
      % Monitor was reduced an action was analysed.
      io:format(">> Normal monitor returned~n"),
      M_
  end.


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
