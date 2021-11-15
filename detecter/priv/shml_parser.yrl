%%% @author Duncan Paul Attard
%%%
%%% Parser specification.
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


%%% ----------------------------------------------------------------------------
%%% Non-terminal and terminal symbols definition.
%%% ----------------------------------------------------------------------------

Nonterminals

% Safety HML.
shml nec act shml_.

% Erlang atomic data structures.
%%atomic strings

Terminals

% Boolean, arithmetic and bitwise operators.
'*' '/' 'div' 'rem' 'band' 'and'
'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'
'andalso' 'orelse'
'bnot' 'not'

% Relational operators.
'==' '/=' '=<' '<' '>=' '>' '=:=' '=/='

% List operators.
'|' '||' '<-' '++' '--'

% Process action operators.
'->' '**' '!' '?'

% Punctuation and meta symbols.
'(' ')' '[' ']' '{' '}' ',' '.' ';' ':'

% Safety HML.
ff tt max

% Keywords.
'with' 'when' 'monitor'

% Atomic data types.
char integer float atom string var.


%%% ----------------------------------------------------------------------------
%%% Grammar definition.
%%% ----------------------------------------------------------------------------

Rootsymbol shml.

%% Safety HML definition.
%% sHML -> tt | ff | [act] sHML | max X. sHML | X

%% Safety HML productions. Ambiguity from the above grammar is removed by
%% reformulating it into a left-recursive grammar whereby conjunctions associate
%% to the left. Bracketing overrides this default left association. See the
%% below on how to refactor grammars to deal with ambiguity in terms of
%% associativity and operator precedence.
%% * https://www.geeksforgeeks.org/removal-of-ambiguity-converting-an-ambiguos-grammar-into-unambiguos-grammar/
%% * http://homepage.divms.uiowa.edu/~jones/compiler/spring13/notes/10.shtml

% Left-associative conjunction.
shml -> shml 'and' shml_ : {'and', '$1', '$3'}.
%%shml -> shml_ 'and' shml : {'and', '$1', '$3'}. % Right-associative alternative.
shml -> shml_ : '$1'.

% Modal necessity.
shml_ -> '[' act ']' shml_ : {nec, '$2', '$4'}.

% Maximal fix-point.
shml_ -> max '(' var '.' shml_ ')' : {max, '$3', '$5'}.

% Truth, falsity and bracketing.
shml_ -> ff : '$1'.
shml_ -> tt : '$1'.
shml_ -> '(' shml ')' : '$2'.









act -> atom : '$1'.






%%% ----------------------------------------------------------------------------
%%% Erlang supporting macros and code.
%%% ----------------------------------------------------------------------------

Erlang code.

%% keep track of annotation info in tokens i.e., extracts the actual content of
%% the token produced by the lexer. In this case it extracts the token line number.
%% {token, InfoOnToken}.
-define(anno(Tuple), element(2, Tuple)).
-define(name(Tuple), element(3, Tuple)).

-define(mkop2(L, OpAnno, R),
  begin
    {Op, Anno} = OpAnno,
    {op, Anno, Op, L, R}
  end).

-define(mkop1(OpAnno, A),
  begin
    {Op, Anno} = OpAnno,
    {op, Anno, Op, A}
  end).



%% Builds the MFA abstract syntax tree.
build_mfa(Mod, Fun, [], []) ->
{mfa, ?anno(Mod), ?name(Mod), ?name(Fun), 0, {clause, ?anno(Mod), [], [], []}};
build_mfa(Mod, Fun, Exprs, ClauseGuard) ->
Arity = length(Exprs),
{mfa, ?anno(Mod), ?name(Mod), ?name(Fun), Arity,
  {clause, ?anno(hd(Exprs)), Exprs, ClauseGuard, []}}.