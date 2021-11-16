%%% @author Duncan Paul Attard
%%%
%%% Lexer specification.
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
%%% Regular expression class definitions.
%%% ----------------------------------------------------------------------------

Definitions.

% Digits.
DIGIT                     = [0-9]

% Lowercase letters.
LOWER                     = [a-z]

% Uppercase letters.
UPPER                     = [A-Z]

% Alpha-numeric letters with underscore and at.
ALPHA                     = [a-zA-Z0-9_@]

% Boolean operators.
BOOL_OP                   = (and|or|andalso|orelse|not|xor)

% Arithmetic operators.
ARIT_OP                   = (\+|-|\*|/|div|rem)

% Bitwise operators.
BITW_OP                   = (bnot|band|bor|bxor|bsl|bsr)

% Relational operators.
COMP_OP                   = (==|/=|=<|<|>=|>|=:=|=/=)

% List operators.
LIST_OP                   = (\||\|\||<-|\+\+|--)

% Binary operators.
BIN_OP                    = (<<|>>|<=)

% Map operators.
MAP_OP                    = (#|:=)

% Process action operators.
PROC_OP                   = (->|\*\*|!|\?)

% Punctuation and meta symbols.
META_OP                   = [\(\)\[\]\{\}\,\.\;\:]

% Whitespace and comments.
WS                        = ([\s\t\n\r]|%.*)


%%% ----------------------------------------------------------------------------
%%% Token definitions.
%%% ----------------------------------------------------------------------------

Rules.

% Boolean operators.
{BOOL_OP}                 : {token, {?to_atom(TokenChars), TokenLine}}.

% Arithmetic operators.
{ARIT_OP}                 : {token, {?to_atom(TokenChars), TokenLine}}.

% Bitwise operators.
{BITW_OP}                 : {token, {?to_atom(TokenChars), TokenLine}}.

% Relational operators.
{COMP_OP}                 : {token, {?to_atom(TokenChars), TokenLine}}.

% List operators.
{LIST_OP}                 : {token, {?to_atom(TokenChars), TokenLine}}.

% Binary operators.
{BIN_OP}                  : {token, {?to_atom(TokenChars), TokenLine}}.

% Map operators.
{MAP_OP}                  : {token, {?to_atom(TokenChars), TokenLine}}.

% Process actions.
{PROC_OP}                 : {token, {?to_atom(TokenChars), TokenLine}}.

% Variables.
_{ALPHA}*|{UPPER}{ALPHA}* : {token, {var, TokenLine, ?to_atom(TokenChars)}}.

% Atoms and keywords.
{LOWER}{ALPHA}*|'[^\']*'  : Atom = ?to_atom(TokenChars),
                            {token, case is_keyword(Atom) of
                                      true -> {Atom, TokenLine};
                                      false -> {atom, TokenLine, Atom}
                                    end
                            }.

% Strings.
"(\\\^.|\\.|[^\"])*"      : {token, {string, TokenLine, ?no_quotes(TokenChars)}}.

% Integers.
{DIGIT}+                  : {token, {integer, TokenLine, ?to_integer(TokenChars)}}.

% Floats.
{DIGIT}+\.{DIGIT}+        : {token, {float, TokenLine, ?to_float(TokenChars)}}.

% Punctuation and meta symbols.
{META_OP}                 : {token, {?to_atom(TokenChars), TokenLine}}.

% Whitespace and comments.
{WS}+                     : skip_token.


%%% ----------------------------------------------------------------------------
%%% Erlang supporting macros and code.
%%% ----------------------------------------------------------------------------

Erlang code.

%%% String conversion macros.
-define(to_atom(String), list_to_atom(String)).
-define(to_integer(String), list_to_integer(String)).
-define(to_float(String), list_to_float(String)).
-define(to_pid(String), list_to_pid(String)).
-define(no_quotes(String), lists:sublist(String, 2, length(String) - 2)).

%%% Keywords.
is_keyword('when') -> true;
is_keyword(with) -> true;
is_keyword(monitor) -> true;

is_keyword(tt) -> true;
is_keyword(ff) -> true;
is_keyword(max) -> true;

is_keyword(_) -> false.
