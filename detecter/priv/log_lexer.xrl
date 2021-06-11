%%% ----------------------------------------------------------------------------
%%% Regular expression classes.
%%% ----------------------------------------------------------------------------

Definitions.

% Digit characters.
DIGIT                     = [0-9]

% Lowercase characters.
LOWER                     = [a-z]

% Uppercase characters.
UPPER                     = [A-Z]

% Alpha-numeric token that includes the underscore symbol.
ALPHA                     = [a-zA-Z0-9_]

% String token.
STRING                    = [!-~\s\t\n\r]

% Whitespace characters.
WS                        = ([\s\t]|%.*)


%%% ----------------------------------------------------------------------------
%%% Lexical token rule definitions.
%%% ----------------------------------------------------------------------------

Rules.

% Keyword tokens.
delayed                   : {token, {delayed, TokenLine}}.
by                        : {token, {by, TokenLine}}.

% Event tokens.
fork                      : {token, {fork, TokenLine}}.
init                      : {token, {init, TokenLine}}.
exit                      : {token, {exit, TokenLine}}.
send                      : {token, {send, TokenLine}}.
recv                      : {token, {recv, TokenLine}}.

% Atom token.
{LOWER}{ALPHA}*           : {token, {atom, TokenLine, ?to_atom(TokenChars)}}.

% Integer token.
-?{DIGIT}+                  : {token, {int, TokenLine, ?to_integer(TokenChars)}}.

% Float token.
-?{DIGIT}+\.{DIGIT}+        : {token, {float, TokenLine, ?to_float(TokenChars)}}.

% PID token.
<{DIGIT}+\.{DIGIT}+\.{DIGIT}+> : {token, {pid, TokenLine, ?to_pid(TokenChars)}}.

% Reference token.
#Ref<{DIGIT}+\.{DIGIT}+\.{DIGIT}+\.{DIGIT}+> : {token, {ref, TokenLine, ?to_ref(TokenChars)}}.

% String token.
"{STRING}*" : {token, {string, TokenLine, TokenChars}}.

% Punctuation and meta symbol tokens.
[()\[\]{}\,\.]              : {token, {?to_atom(TokenChars), TokenLine}}.

% Whitespace and comment tokens.
{WS}+   					        : skip_token.

%%% ----------------------------------------------------------------------------
%%% Erlang supporting macros.
%%% ----------------------------------------------------------------------------

Erlang code.

%% String conversion macros.
-define(to_atom(String), list_to_atom(String)).
-define(to_integer(String), list_to_integer(String)).
-define(to_float(String), list_to_float(String)).
-define(to_pid(String), list_to_pid(String)).
-define(to_ref(String), list_to_ref(String)).
