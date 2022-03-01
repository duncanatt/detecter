%%% @author Duncan Paul Attard
%%%
%%% maxHML parser specification.
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

%%% Maximal HML non-terminals.
forms form act pat mfargs
maxhml_expr maxhml_fact maxhml_term
mfargs_sel

%%% Erlang non-terminals.

% Expressions.
expr expr_100 expr_150 expr_160 expr_200 expr_300 expr_400 expr_500
expr_600 expr_700 expr_800
expr_max

% Composite data structures. Records are not currently supported.
list tail
list_comprehension lc_expr lc_exprs
binary_comprehension
binary bin_elements bin_element bit_expr
opt_bit_size_expr bit_size_expr opt_bit_type_list bit_type_list bit_type
tuple
map_expr map_tuple map_field map_field_exact map_fields map_key

% Atomic data structures.
atomic strings

% Expressions and guards.
exprs guard

% Unary and binary operators.
prefix_op mult_op add_op list_op comp_op.

Terminals

%%% maxHML and formula sequences terminals.
'ff' 'tt' 'max'
'with' 'when' 'check'

%%% Erlang terminals.

% Boolean, arithmetic and bitwise operators.
'*' '/' 'div' 'rem' 'band' 'and'
'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'
'andalso' 'orelse'
'bnot' 'not'

% Relational operators.
'==' '/=' '=<' '<' '>=' '>' '=:=' '=/='

% List operators.
'|' '||' '<-' '++' '--'

% Binary operators.
'<<' '>>' '<='

% Map operators.
'#' ':='

% Process action operators.
'->' '**' '!' '?'

% Punctuation and meta symbols.
'(' ')' '[' ']' '{' '}' ',' '.' ';' ':'

% Atomic data types.
char integer float atom string var.

Rootsymbol forms.


%%% ----------------------------------------------------------------------------
%%% maxHML formula sequences.
%%% ----------------------------------------------------------------------------

% Formula lists.
forms -> form '.'                             : ['$1'].
forms -> form ',' forms                       : ['$1' | '$3'].

% Designating the process MFArgs on which the maxHML formula is to be checked.
% Formulae are interpreted w.r.t. to the traces exhibited by the process forked
% using MFArgs.
form -> 'with' mfargs_sel 'check' maxhml_expr : {form, ?anno('$1'), '$2', '$4'}.
mfargs_sel -> mfargs                          : {sel, ?anno('$1'), '$1', []}.
mfargs_sel -> mfargs 'when' guard             : {sel, ?anno('$1'), '$1', '$3'}.


%%% ----------------------------------------------------------------------------
%%% Maximal HML grammar definition.
%%% maxHML -> tt | ff | <act> maxHML | [act] maxHML
%%%          | maxHML or maxHML | maxHML and maxHML
%%%          | max X. maxHML | X
%%% ----------------------------------------------------------------------------

%%% Maximal HML productions. Ambiguity from the grammar is removed by
%%% reformulating it into a left-recursive grammar whereby disjunctions and
%%% conjunctions associate to the left. Bracketed expressions overrides this
%%% default left association. See the links below on how to refactor grammars to
%%% deal with ambiguity in terms of associativity and operator precedence.
%%% * https://www.geeksforgeeks.org/
%%%   removal-of-ambiguity-converting-an-ambiguos-grammar-into-unambiguos-grammar
%%% * http://homepage.divms.uiowa.edu/~jones/compiler/spring13/notes/10.shtml
%%% * https://opendsa-server.cs.vt.edu/OpenDSA/Books/PL/html/Grammars3.html

% Left-associative disjunction and conjunction.
maxhml_expr -> maxhml_expr 'or' maxhml_term   : {'or', ?anno('$1'), '$1', '$3'}.
maxhml_expr -> maxhml_term                    : '$1'.
maxhml_term -> maxhml_term 'and' maxhml_fact  : {'and', ?anno('$1'), '$1', '$3'}.
maxhml_term -> maxhml_fact                    : '$1'.

% Modal possibility and necessity.
maxhml_fact -> '<' act '>' maxhml_fact        : {pos, ?anno('$1'), '$2', '$4'}.
maxhml_fact -> '[' act ']' maxhml_fact        : {nec, ?anno('$1'), '$2', '$4'}.

% Maximal fix-point.
maxhml_fact -> 'max' var '.' '(' maxhml_expr ')'  : {max, ?anno('$1'), '$2', '$5'}.

% Truth, falsity, recursive variables and bracketing.
maxhml_fact -> 'ff'                           : '$1'.
maxhml_fact -> 'tt'                           : '$1'.
maxhml_fact -> var                            : '$1'.
maxhml_fact -> '(' maxhml_expr ')'            : '$2'.

%%% Data extensions with symbolic action pairs comprised of process action
%%% patterns and decidable Boolean constraints. Five process action patterns are
%%% supported: send, receive, fork, initialisation and termination.

% Symbolic actions. Constraints in symbolic actions are optional, and internally
% interpreted as true when omitted. The implementation of constraints is
% delegated to Erlang guards. The curly braces '{}' around symbolic actions are
% used as an aid to visual parsing. These are also used to resolve a
% shift-reduce conflict that arises due to the angled brackets used for the
% possibility modality and > operator in guards. To be resolved in the future.
act -> '{' pat '}'                  : {act, ?anno('$1'), '$2', []}.
act -> '{' pat 'when' guard '}'     : {act, ?anno('$1'), '$2', '$4'}.

% Process sending pattern. Process in var_1 sent a message to process in var_2.
pat -> var ':' var '!' expr         : {send, ?anno('$1'), '$1', '$3', '$5'}.

% Process receiving pattern. Process in var_1 receives message.
pat -> var '?' expr                 : {recv, ?anno('$1'), '$1', '$3'}.

% Process forking pattern. Process in var_1 forked child in var_2 via MFArgs.
pat -> var '->' var ',' mfargs      : {fork, ?anno('$1'), '$1', '$3', '$5'}.

% Process initialization pattern. Child in var_2 was forked by process in var_1
% via MFArgs.
pat -> var '<-' var ',' mfargs      : {init, ?anno('$1'), '$3', '$1', '$5'}.

% Process termination pattern. Process in var_1 exited with specified reason.
pat -> var '**' expr                : {exit, ?anno('$1'), '$1', '$3'}.

% MFArgs pattern. There are two ways to implement this. One is to use the Erlang
% expression syntax where pattern matching can be performed in place, in the
% parameters themselves. Although using a list of exprs makes it possible to
% write illegal patterns (e.g., A + 10), this is the route taken by Erlang,
% where parsing succeeds and the errors is detected on compilation. The second
% option is to simply use a list of variables and avoid this. We choose the
% former approach since it is more flexible in practice, and as we generate an
% Erlang AST, the Erlang compiler can handle the illegal pattern as usual.
mfargs -> atom ':' atom '(' ')'         : build_mfargs('$1', '$3', []).
mfargs -> atom ':' atom '(' exprs ')'   : build_mfargs('$1', '$3', '$5').


%%% ----------------------------------------------------------------------------
%%% Erlang grammar definition.
%%% ----------------------------------------------------------------------------

%%% Rest of Erlang expressions (with precedence). Certain precedence rules are
%%% left as empty and kept as placeholders in case we want to add more power to
%%% our grammar.

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
expr_800 -> expr_max : '$1'.

% Variables, atomics, lists and list comprehensions, binary and binary
% comprehensions tuples, and bracketed expressions. Begin..end, if, case,
% receive, anonymous function, and try..catch expressions are not currently
% supported.
expr_max -> var : '$1'.
expr_max -> atomic : '$1'.
expr_max -> list : '$1'.
expr_max -> binary : '$1'.
expr_max -> list_comprehension : '$1'.
expr_max -> binary_comprehension : '$1'.
expr_max -> tuple : '$1'.
expr_max -> '(' expr ')' : '$2'.

%%% Composite data structures.

% Lists.
list -> '[' ']' : {nil, ?anno('$1')}.
list -> '[' expr tail : {cons, ?anno('$1'), '$2', '$3'}.

tail -> ']' : {nil, ?anno('$1')}.
tail -> '|' expr ']' : '$2'.
tail -> ',' expr tail : {cons, ?anno('$2'), '$2', '$3'}.

% Binaries.
binary -> '<<' '>>' : {bin,?anno('$1'),[]}.
binary -> '<<' bin_elements '>>' : {bin,?anno('$1'),'$2'}.

bin_elements -> bin_element : ['$1'].
bin_elements -> bin_element ',' bin_elements : ['$1'|'$3'].

bin_element -> bit_expr opt_bit_size_expr opt_bit_type_list :
{bin_element,?anno('$1'),'$1','$2','$3'}.

bit_expr -> prefix_op expr_max : ?mkop1('$1', '$2').
bit_expr -> expr_max : '$1'.

opt_bit_size_expr -> ':' bit_size_expr : '$2'.
opt_bit_size_expr -> '$empty' : default.

opt_bit_type_list -> '/' bit_type_list : '$2'.
opt_bit_type_list -> '$empty' : default.

bit_type_list -> bit_type '-' bit_type_list : ['$1' | '$3'].
bit_type_list -> bit_type : ['$1'].

bit_type -> atom             : element(3,'$1').
bit_type -> atom ':' integer : { element(3,'$1'), element(3,'$3') }.

bit_size_expr -> expr_max : '$1'.

% List and binary comprehensions.
list_comprehension -> '[' expr '||' lc_exprs ']' : {lc, ?anno('$1'), '$2', '$4'}.
binary_comprehension -> '<<' expr_max '||' lc_exprs '>>' : {bc,?anno('$1'),'$2','$4'}.
lc_exprs -> lc_expr : ['$1'].
lc_exprs -> lc_expr ',' lc_exprs : ['$1'|'$3'].

lc_expr -> expr : '$1'.
lc_expr -> expr '<-' expr : {generate, ?anno('$2'), '$1', '$3'}.
lc_expr -> binary '<=' expr : {b_generate,?anno('$2'),'$1','$3'}.

% Tuples.
tuple -> '{' '}' : {tuple, ?anno('$1'), []}.
tuple -> '{' exprs '}' : {tuple, ?anno('$1'), '$2'}.

% Maps. Note that creating associations in maps is disallowed, and only pattern
% matching is supported.
map_expr -> '#' map_tuple : {map, ?anno('$1'),'$2'}.
map_expr -> expr_max '#' map_tuple : {map, ?anno('$2'),'$1','$3'}.
map_expr -> map_expr '#' map_tuple : {map, ?anno('$2'),'$1','$3'}.

map_tuple -> '{' '}' : [].
map_tuple -> '{' map_fields '}' : '$2'.

map_fields -> map_field : ['$1'].
map_fields -> map_field ',' map_fields : ['$1' | '$3'].

map_field -> map_field_exact : '$1'.

map_field_exact -> map_key ':=' expr : {map_field_exact,?anno('$1'),'$1','$3'}.

map_key -> expr : '$1'.

% Literals.
exprs -> expr : ['$1'].
exprs -> expr ',' exprs : ['$1' | '$3'].

% Guards.
guard -> exprs : ['$1'].
guard -> exprs ';' guard : ['$1' | '$3'].

%%% Atomic data structures.

% Primitive data types and strings.
atomic -> char : '$1'.
atomic -> integer : '$1'.
atomic -> float : '$1'.
atomic -> atom : '$1'.
atomic -> strings : '$1'.

strings -> string : '$1'.
strings -> string strings : {string, ?anno('$1'), element(3, '$1') ++ element(3, '$2')}.

%%% Binary and unary operators.

% Unary operators.
prefix_op -> '+'              : '$1'.
prefix_op -> '-'              : '$1'.
prefix_op -> 'bnot'           : '$1'.
prefix_op -> 'not'            : '$1'.

% Binary operators.
mult_op -> '/'                : '$1'.
mult_op -> '*'                : '$1'.
mult_op -> 'div'              : '$1'.
mult_op -> 'rem'              : '$1'.
mult_op -> 'band'             : '$1'.
mult_op -> 'and'              : '$1'.

add_op -> '+'                 : '$1'.
add_op -> '-'                 : '$1'.
add_op -> 'bor'               : '$1'.
add_op -> 'bxor'              : '$1'.
add_op -> 'bsl'               : '$1'.
add_op -> 'bsr'               : '$1'.
add_op -> 'or'                : '$1'.
add_op -> 'xor'               : '$1'.

% List concatenation and difference.
list_op -> '++'               : '$1'.
list_op -> '--'               : '$1'.

% Relational operators.
comp_op -> '=='               : '$1'.
comp_op -> '/='               : '$1'.
comp_op -> '=<'               : '$1'.
comp_op -> '<'                : '$1'.
comp_op -> '>='               : '$1'.
comp_op -> '>'                : '$1'.
comp_op -> '=:='              : '$1'.
comp_op -> '=/='              : '$1'.


%%% ----------------------------------------------------------------------------
%%% Erlang supporting macros and code.
%%% ----------------------------------------------------------------------------

Erlang code.

%% Token information extraction macros {token, TokenInfo}.
-define(anno(Tuple), element(2, Tuple)). % Line number.
-define(name(Tuple), element(3, Tuple)). % Name.

%% Binary and unary operator AST node creation macros.
-define(mkop2(L, OpAnno, R), % Binary operator.
  begin
    {Op, Anno} = OpAnno,
    {op, Anno, Op, L, R}
  end).

-define(mkop1(OpAnno, A), % Unary operator
  begin
    {Op, Anno} = OpAnno,
    {op, Anno, Op, A}
  end).

%% Builds the MFArgs AST node.
build_mfargs(Mod, Fun, Exprs) when is_list(Exprs) ->
  {mfargs, ?anno(Mod), ?name(Mod), ?name(Fun), Exprs}.
