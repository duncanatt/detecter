%%% ----------------------------------------------------------------------------
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
%%% Grammar definitions.
%%% ----------------------------------------------------------------------------

Nonterminals

% Normalized Safety HML.
forms form mfa shml act shml_seq shml_nec shml_nec_seq shml0 shml_seq0 nec shml1 shml2

%% Erlang grammar.

% Erlang clauses.
clause clause_guard

% Erlang expressions.
expr expr_100 expr_150 expr_160 expr_200 expr_300 expr_400 expr_500
expr_600 expr_700 expr_800
expr_max

% Expression patterns.
%%pat_expr pat_expr_200 pat_expr_300 pat_expr_400 pat_expr_500
%%pat_expr_600 pat_expr_700 pat_expr_800
%%pat_expr_max
%%map_pat_expr record_pat_expr
%%pat_argument_list pat_exprs

% Erlang composite data structures.
list tail
list_comprehension lc_expr lc_exprs
%%binary_comprehension
tuple
%%record_expr record_tuple record_field record_fields
%%map_expr map_tuple map_field map_field_assoc map_field_exact map_fields map_key

% Erlang atomic data structures.
atomic strings

% Erlang expressions and guards.
exprs guard

% Erlang unary and binary operators.
prefix_op mult_op add_op list_op comp_op.

Terminals

% Atomic data types.
char integer float atom string var

% Punctuation symbols.
'(' ')' '[' ']' '{' '}' ',' '.' ';' ':'

% List operators.
'|' '||' '<-' '++' '--'

% Comparison operators.
'==' '/=' '=<' '<' '>=' '>' '=:=' '=/='

% Process action operators.
'->' '**' '!' '?'

% Mathematical and boolean operators.
'*' '/' 'div' 'rem' 'band' 'and'
'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'
'andalso' 'orelse'
'bnot' 'not'

% Keywords.
'with' 'when' 'monitor'

% Normalized Safety HML symbols.
'tt' 'ff' 'max'.


Rootsymbol forms.

%%Unary 250 shml.
%%
%%Unary 500 nec.


% Formula sequence grammar.
forms -> form '.'                                   : ['$1'].
forms -> form ',' forms                             : ['$1' | '$3'].

% Localized formula grammar.
%%form -> with mfa monitor shml_seq                   : {form, ?anno('$1'), '$2', '$4'}.
form -> with mfa monitor shml                   : {form, ?anno('$1'), '$2', '$4'}.

% Normalized Safety HML grammar.

% The second production makes it possible to specify formulas that are not
% normalized. This is ok however, and the design decision was taken in favor of
% simpler grammar that is easier to work with. The check for normal form will
% be done as part of a semantic check on the syntax tree.
%%shml_seq -> shml                                    : ['$1'].
%%shml_seq -> shml and shml_seq                       : ['$1' | '$3']. % This does not necessarily produce SHMLNF, because we can have
%%
%%shml -> tt                                          : '$1'.
%%shml -> ff                                          : '$1'.
%%shml -> var                                         : '$1'.
%%shml -> '[' act ']' shml                            : {nec, ?anno('$1'), '$2', '$4'}.
%%shml -> max var '.' shml                            : {max, ?anno('$1'), '$2', '$4'}.


%%% Grammar V1: Devised by Adrian and myself. Although correct, it gives rise to
%%% one shift/reduce conflict relating to the 'and' operator.
%%shml -> ff                                          : '$1'.
%%shml -> shml_seq                                    : '$1'.
%%shml -> var                                         : '$1'.
%%shml -> max var '.' shml                            : {max, ?anno('$1'), '$2', '$4'}.
%%
%%% Ambiguous
%%shml_seq -> '[' var ']' shml and shml_seq           : {'and', {nec, ?anno('$1'), '$2', '$4'}, '$6'}.
%%shml_seq -> '[' var ']' shml                        : {nec, ?anno('$1'), '$2', '$4'}.





%% --- WORKS WELL EVEN WITH BRACKETING --
%%shml -> shml and shml0 : {'and', '$1', '$3'}.
%%shml -> shml0 : '$1'.

%%shml0 -> '[' var ']' ff : {nec, '$2', '$4'}.
%%shml0 -> '[' var ']' shml0 : {nec, '$2', '$4'}.
%%shml0 -> '[' var ']' max var '.' shml0 : {nec, '$2', {max, '$4', '$5', '$7'}}.
%%shml0 -> '(' shml ')' : '$2'.
%% --- WORKS WELL EVEN WITH BRACKETING --




%%% Grammar V2: Implements the restrictions for SHMLnf, correct bracketing and
%%% 'and' association, where the operator 'and' assumes a binary interpretation,
%%% and consequently, returns and AST whose nodes form a binary tree.
%%shml -> ff : '$1'.
%%shml -> max '(' var '.' shml0 ')' : {max, '$3', '$5'}.
%%shml -> shml0 : '$1'.
%%
%%shml0 -> shml0 and nec : {'and', '$1', '$3'}.
%%shml0 -> nec : '$1'.
%%
%%nec -> '[' var ']' shml1 : {nec, '$2', '$4'}.
%%nec -> '[' var ']' nec : {nec, '$2', '$4'}.
%%nec -> '(' shml0 ')' : '$2'.
%%
%%shml1 -> ff : '$1'.
%%shml1 -> var : '$1'.
%%shml1 -> max '(' var '.' shml0 ')' : {max, '$3', '$5'}.

% Grammar V3: Implements fully-fledged SHML without restrictions. Not used here,
% but merely as a note to show to Jasmine in order to remove unnecessary
% bracketing from her parser. To be done later.

% Grammar V4: New verbose, albeit fully-functional version suggested by Adrian
% where the operator 'and' assumes an n-ary interpretation, n = 1 being the
% degenerate case for a single necessity (but which must be, nevertheless,
% specified in the property). Consequently, the 'and' node in the AST is
% represented by a list, instead of a binary tree node.
shml -> ff                                            : '$1'.
shml -> var                                           : '$1'.
shml -> max '(' var '.' shml ')'                      : {max, ?anno('$1'), '$3', '$5'}.
shml -> and '(' shml_seq ')'                          : {'and', ?anno('$1'), length('$3'), '$3'}.

shml_seq -> nec                                       : ['$1'].
shml_seq -> nec ',' shml_seq                          : ['$1' | '$3'].

nec -> '[' act ']' shml                               : {nec, ?anno('$1'), '$2', '$4'}.


























%%% Ambiguous grammar that parses "ff and tt and ff" as {'and',ff,{'and',tt,ff}}}.
%%shml -> shml and shml                                 : {'and', '$1', '$3'}.
%%shml -> ff                                            : ff.
%%shml -> tt                                            : tt.

%%% Unambiguous grammar that parses "ff and tt and ff" as {'and',ff,{'and',tt,ff}}}.
%%shml -> shml0 and shml                                  : {'and', '$1', '$3'}.
%%shml -> shml0                                           : '$1'.
%%shml0 -> ff                                             : ff.
%%shml0 -> tt                                             : tt.

% Unambiguous grammar that parses "ff and tt and ff" as {'and',{'and',ff,tt},ff}}.
%%shml -> shml and shml0                                    : {'and', '$1', '$3'}.
%%shml -> shml0                                             : '$1'.
%%shml0 -> ff                                               : ff.
%%shml0 -> tt                                               : tt.






% My new grammar exactly on Ian's paper verbatim.
%%shml -> ff                                          : ['$1'].
%%shml -> shml_nec                                    : ['$1'].
%%shml -> var                                         : ['$1'].
%%shml -> max var '.' shml                            : [max, ?anno('$1'), '$2', '$4'].
%%
%%shml_nec -> '[' act ']' shml shml_nec_seq           : [{nec, ?anno('$1'), '$2', '$4'} | '$5'].
%%
%%shml_nec_seq -> and shml_nec                        : '$2'.
%%shml_nec_seq -> '$empty'                            : [].

%%and shml_seq           : [{nec, ?anno('$1'), '$2', '$4'} | '$6'].
%%shml_seq -> '$empty'                                : [].




% Process in var performed a send of msg.
act -> var ':' var '!' clause                         : {send, ?anno('$1'), '$1', '$3', '$5'}.

% Process in var performed a receive of msg.
act -> var '?' clause                         : {recv, ?anno('$1'), '$1', '$3'}.

% Process in var terminated with reason.
act -> var '**' clause                        : {exit, ?anno('$1'), '$1', '$3'}.

% Process in var0 forked process in var1 on MFA.
act -> var '->' var ',' mfa                   : {fork, ?anno('$1'), '$1', '$3', '$5'}.

% Process in var1 was forked by process in var0 on MFA.
act -> var '<-' var ',' mfa                   : {init, ?anno('$1'), '$3', '$1', '$5'}.

% Custom defined actions that are not just limited to '!', '?' '**' and '->'.
act -> clause                                 : {user, ?anno('$1'), '$1'}.

% MFA grammar.
% MFA must have at least one expression.
mfa -> atom ':' atom '(' ')'                      : build_mfa('$1', '$3', [], []).
mfa -> atom ':' atom '(' exprs ')' clause_guard   : build_mfa('$1', '$3', '$5', '$7').





% Erlang terms and expression grammar.




% Clause must have exactly one expression.
clause -> expr clause_guard                      : {clause, ?anno('$1'), ['$1'], '$2', []}.


clause_guard -> 'when' guard : '$2'.
clause_guard -> '$empty' : [].


%%expr -> 'catch' expr : {'catch',?anno('$1'),'$2'}.
expr -> expr_100 : '$1'.

%%expr_100 -> expr_150 '=' expr_100 : {match,?anno('$2'),'$1','$3'}.
%%expr_100 -> expr_150 '!' expr_100 : ?mkop2('$1', '$2', '$3').
expr_100 -> expr_150 : '$1'.

expr_150 -> expr_160 'orelse' expr_150 : ?mkop2('$1', '$2', '$3').
expr_150 -> expr_160 : '$1'.

expr_160 -> expr_200 'andalso' expr_160 : ?mkop2('$1', '$2', '$3').
expr_160 -> expr_200 : '$1'.

expr_200 -> expr_300 comp_op expr_300 :
?mkop2('$1', '$2', '$3').
expr_200 -> expr_300 : '$1'.

expr_300 -> expr_400 list_op expr_300 :
?mkop2('$1', '$2', '$3').
expr_300 -> expr_400 : '$1'.

expr_400 -> expr_400 add_op expr_500 :
?mkop2('$1', '$2', '$3').
expr_400 -> expr_500 : '$1'.

expr_500 -> expr_500 mult_op expr_600 :
?mkop2('$1', '$2', '$3').
expr_500 -> expr_600 : '$1'.

expr_600 -> prefix_op expr_700 :
?mkop1('$1', '$2').
%%expr_600 -> map_expr : '$1'.
expr_600 -> expr_700 : '$1'.

%%expr_700 -> function_call : '$1'.
%%expr_700 -> record_expr : '$1'.
expr_700 -> expr_800 : '$1'.

%%expr_800 -> expr_max ':' expr_max : {remote,?anno('$2'),'$1','$3'}.
expr_800 -> expr_max : '$1'.

expr_max -> var : '$1'.
expr_max -> atomic : '$1'.
expr_max -> list : '$1'.
%%expr_max -> binary : '$1'.
expr_max -> list_comprehension : '$1'.
%%expr_max -> binary_comprehension : '$1'.
expr_max -> tuple : '$1'.
expr_max -> '(' expr ')' : '$2'.
%%expr_max -> 'begin' exprs 'end' : {block,?anno('$1'),'$2'}.
%%expr_max -> if_expr : '$1'.
%%expr_max -> case_expr : '$1'.
%%expr_max -> receive_expr : '$1'.
%%expr_max -> fun_expr : '$1'.
%%expr_max -> try_expr : '$1'.

%%pat_argument_list -> '(' ')' : {[],?anno('$1')}.
%%pat_argument_list -> '(' pat_exprs ')' : {'$2',?anno('$1')}.



%%% Pattern expressions.
%%%%pat_expr -> pat_expr_200 '=' pat_expr : {match,?anno('$2'),'$1','$3'}. % TODO: Do we need matching??
%%pat_expr -> pat_expr_200 : '$1'.
%%
%%pat_expr_200 -> pat_expr_300 comp_op pat_expr_300 : ?mkop2('$1', '$2', '$3').
%%pat_expr_200 -> pat_expr_300 : '$1'.
%%
%%pat_expr_300 -> pat_expr_400 list_op pat_expr_300 : ?mkop2('$1', '$2', '$3').
%%pat_expr_300 -> pat_expr_400 : '$1'.
%%
%%pat_expr_400 -> pat_expr_400 add_op pat_expr_500 : ?mkop2('$1', '$2', '$3').
%%pat_expr_400 -> pat_expr_500 : '$1'.
%%
%%pat_expr_500 -> pat_expr_500 mult_op pat_expr_600 : ?mkop2('$1', '$2', '$3').
%%pat_expr_500 -> pat_expr_600 : '$1'.
%%
%%pat_expr_600 -> prefix_op pat_expr_700 : ?mkop1('$1', '$2').
%%%%pat_expr_600 -> map_pat_expr : '$1'.
%%pat_expr_600 -> pat_expr_700 : '$1'.
%%
%%%%pat_expr_700 -> record_pat_expr : '$1'.
%%pat_expr_700 -> pat_expr_800 : '$1'.
%%
%%pat_expr_800 -> pat_expr_max : '$1'.
%%
%%pat_expr_max -> var : '$1'.
%%pat_expr_max -> atomic : '$1'.
%%pat_expr_max -> list : '$1'.
%%%%pat_expr_max -> binary : '$1'.
%%pat_expr_max -> tuple : '$1'.
%%pat_expr_max -> '(' pat_expr ')' : '$2'.
%%
%%%%map_pat_expr -> '#' map_tuple : {map, ?anno('$1'),'$2'}.
%%%%map_pat_expr -> pat_expr_max '#' map_tuple : {map, ?anno('$2'),'$1','$3'}.
%%%%map_pat_expr -> map_pat_expr '#' map_tuple : {map, ?anno('$2'),'$1','$3'}.
%%
%%%%record_pat_expr -> '#' atom '.' atom : {record_index,?anno('$1'),element(3, '$2'),'$4'}.
%%%%record_pat_expr -> '#' atom record_tuple : {record,?anno('$1'),element(3, '$2'),'$3'}.


% Composite data structures.

list -> '[' ']' : {nil, ?anno('$1')}.
list -> '[' expr tail : {cons, ?anno('$1'), '$2', '$3'}.

tail -> ']' : {nil, ?anno('$1')}.
tail -> '|' expr ']' : '$2'.
tail -> ',' expr tail : {cons, ?anno('$2'), '$2', '$3'}.


%%binary -> '<<' '>>' : {bin,?anno('$1'),[]}.
%%binary -> '<<' bin_elements '>>' : {bin,?anno('$1'),'$2'}.

%%bin_elements -> bin_element : ['$1'].
%%bin_elements -> bin_element ',' bin_elements : ['$1'|'$3'].

%%bin_element -> bit_expr opt_bit_size_expr opt_bit_type_list :
%%{bin_element,?anno('$1'),'$1','$2','$3'}.

%%bit_expr -> prefix_op expr_max : ?mkop1('$1', '$2').
%%bit_expr -> expr_max : '$1'.

%%opt_bit_size_expr -> ':' bit_size_expr : '$2'.
%%opt_bit_size_expr -> '$empty' : default.

%%opt_bit_type_list -> '/' bit_type_list : '$2'.
%%opt_bit_type_list -> '$empty' : default.

%%bit_type_list -> bit_type '-' bit_type_list : ['$1' | '$3'].
%%bit_type_list -> bit_type : ['$1'].

%%bit_type -> atom             : element(3,'$1').
%%bit_type -> atom ':' integer : { element(3,'$1'), element(3,'$3') }.

%%bit_size_expr -> expr_max : '$1'.


list_comprehension -> '[' expr '||' lc_exprs ']' : {lc, ?anno('$1'), '$2', '$4'}.
%%binary_comprehension -> '<<' expr_max '||' lc_exprs '>>' : {bc,?anno('$1'),'$2','$4'}.
lc_exprs -> lc_expr : ['$1'].
lc_exprs -> lc_expr ',' lc_exprs : ['$1'|'$3'].

lc_expr -> expr : '$1'.
lc_expr -> expr '<-' expr : {generate, ?anno('$2'), '$1', '$3'}.
%%lc_expr -> binary '<=' expr : {b_generate,?anno('$2'),'$1','$3'}.

tuple -> '{' '}' : {tuple, ?anno('$1'), []}.
tuple -> '{' exprs '}' : {tuple, ?anno('$1'), '$2'}.

%%map_expr -> '#' map_tuple : {map, ?anno('$1'),'$2'}.
%%map_expr -> expr_max '#' map_tuple : {map, ?anno('$2'),'$1','$3'}.
%%map_expr -> map_expr '#' map_tuple : {map, ?anno('$2'),'$1','$3'}.

%%map_tuple -> '{' '}' : [].
%%map_tuple -> '{' map_fields '}' : '$2'.

%%map_fields -> map_field : ['$1'].
%%map_fields -> map_field ',' map_fields : ['$1' | '$3'].

%%map_field -> map_field_assoc : '$1'.
%%map_field -> map_field_exact : '$1'.

%%map_field_assoc -> map_key '=>' expr : {map_field_assoc,?anno('$1'),'$1','$3'}.

%%map_field_exact -> map_key ':=' expr : {map_field_exact,?anno('$1'),'$1','$3'}.

%%map_key -> expr : '$1'.


% Literal expressions.
exprs -> expr : ['$1'].
exprs -> expr ',' exprs : ['$1' | '$3'].

% Guards.
guard -> exprs : ['$1'].
guard -> exprs ';' guard : ['$1' | '$3'].


% Atomic data structures.
atomic -> char : '$1'.
atomic -> integer : '$1'.
atomic -> float : '$1'.
atomic -> atom : '$1'.
atomic -> strings : '$1'.

strings -> string : '$1'.
strings -> string strings : {string, ?anno('$1'), element(3, '$1') ++ element(3, '$2')}.


% Operators.
prefix_op -> '+' : '$1'.
prefix_op -> '-' : '$1'.
prefix_op -> 'bnot' : '$1'.
prefix_op -> 'not' : '$1'.

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

list_op -> '++' : '$1'.
list_op -> '--' : '$1'.

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



%% Builds the MFA abstract syntax tree.
build_mfa(Mod, Fun, [], []) ->
  {mfa, ?anno(Mod), ?name(Mod), ?name(Fun), 0, {clause, ?anno(Mod), [], [], []}};
build_mfa(Mod, Fun, Exprs, ClauseGuard) ->
  Arity = length(Exprs),
  {mfa, ?anno(Mod), ?name(Mod), ?name(Fun), Arity,
    {clause, ?anno(hd(Exprs)), Exprs, ClauseGuard, []}}.


%%    {clause, ?anno('$1'), '$1', '$2', []}


%%hml_eval:eval_string("with mod:fun(X, Y, Z) when X > 0, Y =:= \"hello\"; Z =< 20.5 monitor max X.[Pid ! {req, B}]ff and [Pid2 ? {resp, A}]X").