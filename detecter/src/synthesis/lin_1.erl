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
-module(lin_1).
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
-define(MON_KEY, '$mon').


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

mon() ->
  {ok,

    fun X() -> % This is one function.
      ?TRACE("Unfolded variable X."),
      {fun(A) when A =:= 1 ->
        ?TRACE("Rejection verdict on ~p.", [A]),
        no; % Verdict.
        (_A) ->
          ?TRACE("Acceptance verdict on ~p.", [_A]),
          yes % No match.
       end,
        fun(B) when B =:= -1 ->
          X(); % Rec.
          (_B) ->
            ?TRACE("Acceptance verdict on ~p.", [_B]),
            yes % No match.
        end}
    end()
  }.

% This monitor seems to work.
mon2() ->
  {ok,
    fun X() -> % Rec X
      ?TRACE("Unfolded variable X."),
      fun(A) -> % Nec
        ?TRACE("SA ~p, true.", [A]),
        {fun Y() -> % Rec
          ?TRACE("Unfolded variable Y."),
          {
            fun(B) when A =:= B ->
              ?TRACE("SA ~p, ~p =:= ~p.", [B, A, B]),
              no;
              (_B) ->
                ?TRACE("SA (complement) ~p, ~p =/= ~p.", [_B, A, _B]),
                yes
            end,
            fun(B) when A =/= B ->
              ?TRACE("SA ~p, ~p =/= ~p.", [B, A, B]),
              Y();
              (_B) ->
                ?TRACE("SA (complement) ~p, ~p =:= ~p.", [_B, A, _B]),
                yes
            end
          }
         end(), X()}; % Tau of rec Y
        (_A) ->
          ?TRACE("SA complement ~p, false.", [_A]),
          yes
      end
    end() % Tau of rec X
  }.

mon3() ->
  {ok,
    fun X() -> % Rec X
      ?TRACE("Unfolded variable X."),
      fun(A) -> % Nec
        ?TRACE("SA ~p, true.", [A]),
        {'and',
          {fun Y() -> % Rec
            ?TRACE("Unfolded variable Y."),
            {'and',
              {fun(B) when A =:= B ->
                ?TRACE("SA ~p, ~p =:= ~p.", [B, A, B]),
                no;
                (_B) ->
                  ?TRACE("SA (complement) ~p, ~p =/= ~p.", [_B, A, _B]),
                  yes
               end,
                fun(B) when A =/= B ->
                  ?TRACE("SA ~p, ~p =/= ~p.", [B, A, B]),
                  Y();
                  (_B) ->
                    ?TRACE("SA (complement) ~p, ~p =:= ~p.", [_B, A, _B]),
                    yes
                end
              }}
           end(), X()}}; % Tau of rec Y
        (_A) ->
          ?TRACE("SA complement ~p, false.", [_A]),
          yes
      end
    end() % Tau of rec X
  }.


mon4() ->
  {ok,
    {rec,
      fun X() -> % Rec X
        ?TRACE("Unfolded variable X."),
        fun(A) -> % Nec
          ?TRACE("SA ~p, true.", [A]),
          {'and', {
            {rec, fun Y() -> % Rec
              ?TRACE("Unfolded variable Y."),
              {'and',
                {fun(B) when A =:= B ->
                  ?TRACE("SA ~p, ~p =:= ~p.", [B, A, B]),
                  no;
                  (_B) ->
                    ?TRACE("SA (complement) ~p, ~p =/= ~p.", [_B, A, _B]),
                    yes
                 end,
                  fun(B) when A =/= B ->
                    ?TRACE("SA ~p, ~p =/= ~p.", [B, A, B]),
                    {rec, Y};
                    (_B) ->
                      ?TRACE("SA (complement) ~p, ~p =:= ~p.", [_B, A, _B]),
                      yes
                  end
                }}
                  end}, {rec, X}
          }}; % Tau of rec Y
          (_A) ->
            ?TRACE("SA complement ~p, false.", [_A]),
            yes
        end
      end} % Tau of rec X
  }.


mon5() ->
  {ok,
    {rec,
      begin
        ?TRACE("rec x.(a,true.(rec y.(b,a=:=b.no * b,a=/=b.y) * x))"),
        fun X() -> % Rec X
          ?TRACE("a,true.(rec y.(b,a=:=b.no * b,a=/=b.y) * x)"),
          fun(A) -> % Nec
            {'and', {
              {rec,
                begin
                  ?TRACE("rec y.(b,~w=:=b.no * b,~w=/=b.y) * x", [A, A]),
                  fun Y() -> % Rec
                    ?TRACE("b,~w=:=b.no * b,~w=/=b.y", [A, A]),
                    {'and',
                      {fun(B) when A =:= B ->
                        ?TRACE("b,~w=:=~w.no", [A, B]),
                        no;
                        (_B) ->
                          ?TRACE("b,~w=/=~w -> yes", [A, _B]),
                          yes
                       end,
                        fun(B) when A =/= B ->
                          ?TRACE("b,~w=/=~w.y", [A, B]),
                          {recx, Y};
                          (_B) ->
                            ?TRACE("b,~w=:=~w -> yes", [A, _B]),
                            yes
                        end
                      }}
                  end
                end
              }, {recx, X}
            }}; % Tau of rec Y
            (_A) ->
              ?TRACE("SA complement ~p, false.", [_A]),
              yes
          end
        end
      end
    } % Tau of rec X
  }.

mon_simp() ->
  {ok,
    {rec,
      begin
        ?TRACE("rec y.((a,a=:=1.no + a,a=/=1.yes) * (a,a=/=1.y + a,a=:=1.yes))"),
        fun Y() ->
          ?TRACE("(a,a=:=1.no + a,a=/=1.yes) * (a,a=/=1.y + a,a=:=1.yes)"),
          {'and',
            {fun(A) when A =:= 1 ->
              ?TRACE("a,~w=:=1.no", [A]),
              no;
              (_A) ->
                ?TRACE("a,~w=/=1.yes", [_A]),
                yes
             end,
              fun(A) when A =/= 1 ->
                ?TRACE("a,~w=/=1.y", [A]),
                {rec, Y};
                (_A) ->
                  ?TRACE("a,~w=:=1.yes", [_A]),
                  yes
              end}
          }
        end
      end}
  }.

% Mon tau so as to use mTauL and see it in action.
mon_tau() ->
  {ok,
    % Wrap ands and ors with begin and end when we want to add tracing
    % instructions. Or otherwise add the string as an element of the tuple.
    begin
      ?TRACE("a,a=:=1.no + a,a=:=1.yes * rec (x. (a,a=:=2.no + a,a=/=2.yes) * x)"),
      {'and',
        {fun(A) when A =:= 1 ->
          ?TRACE("a,~w=:=1.no", [A]),
          no;
          (_A) ->
            ?TRACE("a,~w=:=1.yes", [_A]),
            yes
         end,
          {rec,
            begin
              ?TRACE("rec (x. (a,a=:=2.no + a,a=/=2.yes) * x)"),
              fun X() ->
                ?TRACE("(a,a=:=2.no + a,a=/=2.yes) * x"),
                {'and',
                  fun(A) when A =:= 2 ->
                    ?TRACE("a,~w=:=2.no", [A]),
                    no;
                    (_A) ->
                      ?TRACE("a,~w=/=2.yes", [_A]),
                      yes
                  end,
                  {rec,
                    begin
                      ?TRACE("x"),
                      X
                    end
                  }
                }
              end
            end}}
      }
    end
  }.


save(M) ->
  put(?MON_KEY, M).

% Function that implements semantic rules.
% For now monitor will be kept in the PD.
sem(Act) ->


%%  {'and',{'and',#Fun<lin_1.3.95595125>,#Fun<lin_1.4.95595125>},#Fun<lin_1.1.95595125>}

  Mon = get(?MON_KEY),
  print_mon(),

  case get(?MON_KEY) of

    {'or', {yes, M}} -> % mDis1L
      ?INFO(":: Applying rule mDisYL."),
      put(?MON_KEY, yes);

    {'or', {M, yes}} -> % mDis1R
      ?INFO(":: Applying rule mDisYR."),
      put(?MON_KEY, yes);

    {'or', {no, M}} -> % mDis2L
      ?INFO(":: Applying rule mDisNL."),
      put(?MON_KEY, M);

    {'or', {M, no}} -> % mDis2R
      ?INFO(":: Applying rule mDisNR."),
      put(?MON_KEY, M);

    {'and', {yes, M}} -> % mCon1L
      ?INFO(":: Applying rule mConYL."),
      put(?MON_KEY, M);

    {'and', {M, yes}} -> % mCon1R
      ?INFO(":: Applying rule mConYR."),
      put(?MON_KEY, M);

    {'and', {no, M}} -> % mCon2L
      ?INFO(":: Applying rule mConNL."),
      put(?MON_KEY, no);


    {'and', {M, no}} -> % mCon2R
      ?INFO(":: Applying rule mConNR."),
      put(?MON_KEY, no);

    {_, {M, N}} when is_function(M), is_function(N) ->

      ?INFO(":: Applying rule mPar."),

      % Both are functions and can be advanced at the same time.
      ?TRACE("M is the function: ~p", [M]),
      ?TRACE("N is the function: ~p", [N]);


%%      {'and', sem()}


    {_, {M, N}} ->

      ?INFO("Rule 'Not quite there yet'."),
      % Apply the action to M and N, and to the substructures of both M and N
      % are not just functions but tuples.


      % One of these is a tuple, and must be reduced further to a monitor.
      ?TRACE("M is possibly a tuple: ~p", [M]),
      ?TRACE("N is possibly a tuple: ~p", [N]),

      put(?MON_KEY, M),
      print_mon(),

      sem(Act),
      print_mon(),

      put(?MON_KEY, N),
      print_mon();



    M when is_function(M) -> % mAct
      ?INFO(":: Applying rule mAct."),
      put(?MON_KEY, M(Act)),
      print_mon();

    M when M =:= yes; M =:= no -> % mVrd
      ?INFO(":: Applying rule mVrd."),
      put(?MON_KEY, M),
      print_mon()
  end.


print_mon() ->
  io:format("----> reduction: ~w.~n~n", [get(?MON_KEY)]).


% Process is as follows:
% 1. When you apply an axiom you return immediately, because it.s the base case,
%    and for sure, you cannot recurse.


% TODO: I Need to update these rules to correctly evolve the monitor.
%%rule(_, {'or', {yes, M}}) -> % Tau
%%  ?INFO(":: Applying rule mDisYL"),
%%  M;
%%rule(_, {'or', {M, yes}}) -> % Tau
%%  ?INFO(":: Applying rule mDisYR"),
%%  M;
%%rule(_, {'or', {no, _}}) -> % Tau
%%  ?INFO(":: Applying rule mDisNL"),
%%  no;
%%rule(_, {'or', {_, no}}) -> % Tau
%%  ?INFO(":: Applying rule mDisNR"),
%%  no;

%%{'and',{{'and',{yes,#Fun<lin_1.4.43215928>}}, #Fun<lin_1.1.43215928>}}


%% AXIOMS ---
rule(Act, Red = {'and', {yes, M}}) -> % Tau
  ?INFO(":: Applying rule mConYL on: ~p.", [Red]),
  M;

rule(Act, Red = {'and', {M, yes}}) -> % Tau
  ?INFO(":: Applying rule mConYR on: ~p.", [Red]),
  M;

rule(_, Red = {'and', {no, _}}) -> % Tau
  ?INFO(":: Applying rule mConNL on: ~p.", [Red]),
  no;
rule(_, Red = {'and', {_, no}}) -> % Tau
  ?INFO(":: Applying rule mConNR on: ~p.", [Red]),
  no;

rule(_, Red = {rec, M}) -> % Tau
  ?INFO(":: Applying rule mRec on: ~p.", [Red]),
%%  MPrime = M(),
%%  rule(Act, MPrime);
%%  rule(Act, MPrime);
  M();

%% AXIOMS ---

%%rule(Act, Red = {recx, M}) -> % Tau
%%  ?INFO(":: Applying rule mRecXXX on: ~p.", [Red]),
%%  MPrime = M(),
%%  rule(Act, MPrime);
%%  M;


% MTauL

rule(Act, Red = {Op, {M = {_Op, _}, N}}) when Op =:= 'and'; Op =:= 'or' ->
  ?INFO(":: Applying rule mTauL (~s) on ~p.", [_Op, Red]),
  MPrime = rule(Act, M),
  rule(Act, {Op, {MPrime, N}});

% Need to fill in mTauR





rule(Act, Red = {Op, {M = {rec, _}, N}}) -> % MTauL
  ?INFO(":: Applying rule mTauL (rec) on ~p.", [Red]),
  MPrime = rule(Act, M),
  rule(Act, {Op, {MPrime, N}});



rule(Act, Red = {Op, {M, N = {rec, _}}}) -> % MTauR
  ?INFO(":: Applying rule mTauR (rec) on ~p.", [Red]),
  NPrime = rule(Act, N),
  rule(Act, {Op, {M, NPrime}});


% Do I need another rule for when they both tau together? Probably not, because
% we first handle left, then right tauing.


%%{'and',{#Fun<lin_1.4.52436259>, #Fun<lin_1.1.52436259>}}
rule(Act, Red = {Op, {M, N}}) when is_function(M), is_function(N) -> % Otherwise both must be functions ready to receive an action.
  ?INFO(":: Applying rule mPar on ~p.", [Red]),


%%  NewMon = {Op, {M(Act), N(Act)}},
  NewMon = {Op, {rule(Act, M), rule(Act, N)}},

  ?INFO("NewMon after MPar = ~p.", [NewMon]),

  NewMon;







rule(_, V) when V =:= yes; V =:= no ->
  ?INFO(":: Applying rule mVer on ~p.", [V]),
  V;

rule(Act, M) when is_function(M) ->
  ?INFO(":: Applying rule mAct on ~p.", [M]),
  M(Act).


% Some mechanism in the return value of the function sem to determine when an
% axiom or a rule was applied. When an axiom is applied, it does not consume the
% action; a rule however, consumes the action. Also, rules that perform tau
% transitions do not consume the action, so beware.

%{tau, M}
%{act, M}.

% Axioms.
sem(Act, Red = {'and', {yes, M}}) ->
  ?INFO(":: Applying axiom mConYL on: ~p.", [Red]),
  {Act, M};

sem(Act, Red = {'and', {no, M}}) ->
  ?INFO(":: Applying axiom mConNL on: ~p.", [Red]),
  {Act, no};

sem(Act, Red = {rec, M}) ->
  ?INFO(":: Applying axiom mRec on: ~p.", [Red]),
  {Act, M()};

sem(_, V) when V =:= yes; V =:= no ->
  ?INFO(":: Applying axiom mVer on ~p.", [V]),
  V; % Act is discarded in this case.

% Rules.
% TAUL and TAUR.





sem(Act, Red = {Op, {M, N}}) when is_function(M, 1), is_function(N, 1) ->
  ?INFO(":: Applying rule mPar on ~p.", [Red]),

  {Op, {sem(Act, M), sem(Act, N)}};


sem(Act, {Op, {{rec, M}, N}}) ->

  % I know for sure that this results in a tau.
%%  {Act, } = sem()
  ok;

sem(Act, {Op, {M, N}}) ->

  ok;

sem(Act, M) when is_function(M, 1) ->
  ?INFO(":: Applying rule mAct on ~p.", [M]),
  % This includes the automatic external choice by virtue of guards.
  M(Act).


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------
