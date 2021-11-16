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

%% Safety HML non-terminals.
shml act shml_ mfa

%% Erlang non-terminals.

% Clauses.
clause clause_guard

% Expressions.
expr expr_100 expr_150 expr_160 expr_200 expr_300 expr_400 expr_500
expr_600 expr_700 expr_800
expr_max

% Composite data structures.
list tail
list_comprehension lc_expr lc_exprs
%%binary_comprehension
tuple
%%record_expr record_tuple record_field record_fields
map_expr map_tuple map_field map_field_exact map_fields map_key

% Atomic data structures.
atomic strings

% Expressions and guards.
exprs guard

% Unary and binary operators.
prefix_op mult_op add_op list_op comp_op.

Terminals

%% Safety HML terminals.
ff tt max
'with' 'when' 'monitor'

%% Erlang terminals.

% Boolean, arithmetic and bitwise operators.
'*' '/' 'div' 'rem' 'band' 'and'
'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'
'andalso' 'orelse'
'bnot' 'not'

% Relational operators.
'==' '/=' '=<' '<' '>=' '>' '=:=' '=/='

% List operators.
'|' '||' '<-' '++' '--'

% Map operators.
'#' ':='

% Process action operators.
'->' '**' '!' '?'

% Punctuation and meta symbols.
'(' ')' '[' ']' '{' '}' ',' '.' ';' ':'

% Atomic data types.
char integer float atom string var.


%%% ----------------------------------------------------------------------------
%%% Safety HML grammar definition.
%%% ----------------------------------------------------------------------------

Rootsymbol shml.

%% Safety HML definition lifted from paper.
%% sHML -> tt | ff | [act] sHML | max X. sHML | X

%% Safety HML productions. Ambiguity from the above grammar is removed by
%% reformulating it into a left-recursive grammar whereby conjunctions associate
%% to the left. Bracketed expressions overrides this default left association.
%% See the below on how to refactor grammars to deal with ambiguity in terms of
%% associativity and operator precedence.
%% * https://www.geeksforgeeks.org/
%%   removal-of-ambiguity-converting-an-ambiguos-grammar-into-unambiguos-grammar
%% * http://homepage.divms.uiowa.edu/~jones/compiler/spring13/notes/10.shtml

% Left-associative conjunction.
shml -> shml 'and' shml_ : {'and', ?anno('$1'), '$1', '$3'}.
%%shml -> shml_ 'and' shml : {'and', ?anno('$1'), '$1', '$3'}. % Right-associative alternative.
shml -> shml_ : '$1'.

% Modal necessity.
shml_ -> '[' act ']' shml_ : {nec, ?anno('$1'), '$2', '$4'}.

% Maximal fix-point.
shml_ -> max '(' var '.' shml_ ')' : {max, ?anno('$1'), '$3', '$5'}.

% Truth, falsity and bracketing.
shml_ -> ff : '$1'.
shml_ -> tt : '$1'.
shml_ -> '(' shml ')' : '$2'.

%% Process actions. In addition to message sending and receiving, process
%% forking, initialization and termination, custom user-defined actions are also
%% permitted.

% Process sending. The process in var_1 sent a message to the process in var_2.
act -> var ':' var '!' clause                         : {send, ?anno('$1'), '$1', '$3', '$5'}.

% Process receiving. The process in var_1 received a message.
act -> var '?' clause                         : {recv, ?anno('$1'), '$1', '$3'}.

% Process forking. The process in var_1 forked the child in var_2 via MFA.
act -> var '->' var ',' mfa                   : {fork, ?anno('$1'), '$1', '$3', '$5'}.

% Process initialization. The process in var_2 was forked by the process in
% var_1 via MFA.
act -> var '<-' var ',' mfa                   : {init, ?anno('$1'), '$3', '$1', '$5'}.

% Process termination. The process in var_1 exited with the specified reason.
act -> var '**' clause                        : {exit, ?anno('$1'), '$1', '$3'}.

% User-defined action. The clause can be any legal Erlang clause.
act -> clause                                 : {user, ?anno('$1'), '$1'}.

%% MFArgs: Module:function(Args)
mfa -> atom ':' atom '(' ')'                      : build_mfargs('$1', '$3', [], []).
mfa -> atom ':' atom '(' exprs ')' clause_guard   : build_mfargs('$1', '$3', '$5', '$7').


%%% ----------------------------------------------------------------------------
%%% Erlang grammar definition.
%%% ----------------------------------------------------------------------------

%% Clauses.

% Clause with one expression.
clause -> expr clause_guard                      : {clause, ?anno('$1'), ['$1'], '$2', []}.

% Clause guard.
clause_guard -> 'when' guard : '$2'.
clause_guard -> '$empty' : [].

%% Expressions with precedence. Certain precedence rules are left as empty
%% placeholders in case we want to add more power to our grammar, such as
%% pattern matching and variable assignment.

% Placeholder for catch expressions.
expr -> expr_100 : '$1'.

% Placeholder for pattern matching and message sending expressions.
expr_100 -> expr_150 : '$1'.

% Short-circuited disjunction.
expr_150 -> expr_160 'orelse' expr_150 : ?mkop2('$1', '$2', '$3').
expr_150 -> expr_160 : '$1'.

% Short-circuited conjunction.
expr_160 -> expr_200 'andalso' expr_160 : ?mkop2('$1', '$2', '$3'). %
expr_160 -> expr_200 : '$1'.

% Relational operators.
expr_200 -> expr_300 comp_op expr_300 : ?mkop2('$1', '$2', '$3').
expr_200 -> expr_300 : '$1'.

% List concatenation and difference.
expr_300 -> expr_400 list_op expr_300 : ?mkop2('$1', '$2', '$3').
expr_300 -> expr_400 : '$1'.

% Arithmetic addition and subtraction, and Boolean operators with the same
% precedence.
expr_400 -> expr_400 add_op expr_500 : ?mkop2('$1', '$2', '$3').
expr_400 -> expr_500 : '$1'.

% Arithmetic multiplication, floating point and integral division, modulo, and
% Boolean operators with the same precedence.
expr_500 -> expr_500 mult_op expr_600 : ?mkop2('$1', '$2', '$3').
expr_500 -> expr_600 : '$1'.

% Unary positive and negative signs, and Boolean operators with the same
% precedence.
expr_600 -> prefix_op expr_700 : ?mkop1('$1', '$2').
expr_600 -> map_expr : '$1'.
expr_600 -> expr_700 : '$1'.

% Placeholder for function calls and record expressions.
expr_700 -> expr_800 : '$1'.

% Placeholder for expressions with the maximum precedence.
%%expr_800 -> expr_max ':' expr_max : {remote,?anno('$2'),'$1','$3'}.
expr_800 -> expr_max : '$1'.

% Variables, atomics, lists and list comprehensions, tuples, and bracketed
% expressions. Binary, binary comprehension, begin..end, if, case, receive,
% anonymous function, and try..catch expressions are not curretly supported.
expr_max -> var : '$1'.
expr_max -> atomic : '$1'.
expr_max -> list : '$1'.
expr_max -> list_comprehension : '$1'.
expr_max -> tuple : '$1'.
expr_max -> '(' expr ')' : '$2'.

%% Composite data structures.

% Lists.
list -> '[' ']' : {nil, ?anno('$1')}.
list -> '[' expr tail : {cons, ?anno('$1'), '$2', '$3'}.

tail -> ']' : {nil, ?anno('$1')}.
tail -> '|' expr ']' : '$2'.
tail -> ',' expr tail : {cons, ?anno('$2'), '$2', '$3'}.

% List comprehensions.
list_comprehension -> '[' expr '||' lc_exprs ']' : {lc, ?anno('$1'), '$2', '$4'}.
lc_exprs -> lc_expr : ['$1'].
lc_exprs -> lc_expr ',' lc_exprs : ['$1'|'$3'].

lc_expr -> expr : '$1'.
lc_expr -> expr '<-' expr : {generate, ?anno('$2'), '$1', '$3'}.

% Tuples.
tuple -> '{' '}' : {tuple, ?anno('$1'), []}.
tuple -> '{' exprs '}' : {tuple, ?anno('$1'), '$2'}.

% Binaries.


% Maps. Creating associations in maps is disallowed.
map_expr -> '#' map_tuple : {map, ?anno('$1'),'$2'}.
map_expr -> expr_max '#' map_tuple : {map, ?anno('$2'),'$1','$3'}.
map_expr -> map_expr '#' map_tuple : {map, ?anno('$2'),'$1','$3'}.

map_tuple -> '{' '}' : [].
map_tuple -> '{' map_fields '}' : '$2'.

map_fields -> map_field : ['$1'].
map_fields -> map_field ',' map_fields : ['$1' | '$3'].

%%map_field -> map_field_assoc : '$1'.
map_field -> map_field_exact : '$1'.

%%map_field_assoc -> map_key '=>' expr : {map_field_assoc,?anno('$1'),'$1','$3'}.

map_field_exact -> map_key ':=' expr : {map_field_exact,?anno('$1'),'$1','$3'}.

map_key -> expr : '$1'.



% Literals.
exprs -> expr : ['$1'].
exprs -> expr ',' exprs : ['$1' | '$3'].

% Guards.
guard -> exprs : ['$1'].
guard -> exprs ';' guard : ['$1' | '$3'].

%% Atomic data structures.

% Primitive data types and strings.
atomic -> char : '$1'.
atomic -> integer : '$1'.
atomic -> float : '$1'.
atomic -> atom : '$1'.
atomic -> strings : '$1'.

strings -> string : '$1'.
strings -> string strings : {string, ?anno('$1'), element(3, '$1') ++ element(3, '$2')}.

%% Binary and unary operators.

% Unary operators.
prefix_op -> '+' : '$1'.
prefix_op -> '-' : '$1'.
prefix_op -> 'bnot' : '$1'.
prefix_op -> 'not' : '$1'.

% Binary operators.
mult_op -> '/' : '$1'.
mult_op -> '*' : '$1'.
mult_op -> 'div' : '$1'.
mult_op -> 'rem' : '$1'.
mult_op -> 'band' : '$1'.
mult_op -> 'and' : '$1'.

add_op -> '+' : '$1'.
add_op -> '-' : '$1'.
add_op -> 'bor' : '$1'.
add_op -> 'bxor' : '$1'.
add_op -> 'bsl' : '$1'.
add_op -> 'bsr' : '$1'.
add_op -> 'or' : '$1'.
add_op -> 'xor' : '$1'.

% List concatenation and difference.
list_op -> '++' : '$1'.
list_op -> '--' : '$1'.

% Relational operators.
comp_op -> '==' : '$1'.
comp_op -> '/=' : '$1'.
comp_op -> '=<' : '$1'.
comp_op -> '<' : '$1'.
comp_op -> '>=' : '$1'.
comp_op -> '>' : '$1'.
comp_op -> '=:=' : '$1'.
comp_op -> '=/=' : '$1'.




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



%% Builds the MFArgs AST.
build_mfargs(Mod, Fun, [], []) ->
  {mfargs, ?anno(Mod), ?name(Mod), ?name(Fun), 0, {clause, ?anno(Mod), [], [], []}};
build_mfargs(Mod, Fun, Exprs, ClauseGuard) when is_list(Exprs) ->
  Arity = length(Exprs),
  {mfargs, ?anno(Mod), ?name(Mod), ?name(Fun), Arity,
    {clause, ?anno(hd(Exprs)), Exprs, ClauseGuard, []}}.

