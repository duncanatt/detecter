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
-module(maxhml_eval).
-author("Duncan Paul Attard").

%%-compile(export_all).

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
%%-include_lib("syntax_tools/include/merl.hrl").
-include("log.hrl").

%%% Public API.
-export([compile/2]).
-export([parse_string/1, parse_file/1]).

%%% Callbacks/Internal.
-export([visit/2]).

%%% Types.
-export_type([af_maxhml/0]).

%%% Implemented behaviors.
-behavior(gen_eval).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% maxHML lexer and parser modules.
-define(LEXER_MOD, maxhml_lexer).
-define(PARSER_MOD, maxhml_parser).

%% maxHML logic AST node tags.
-define(HML_TRU, tt).
-define(HML_FLS, ff).
-define(HML_ACT, act).
-define(HML_POS, pos).
-define(HML_NEC, nec).
-define(HML_OR, 'or').
-define(HML_AND, 'and').
-define(HML_MAX, max).
-define(HML_VAR, var).
-define(MFARGS, mfargs).

%% Monitor AST node tags.
-define(MON_ACC, yes).
-define(MON_REJ, no).
-define(MON_ACT, act).
-define(MON_CHS, chs).
-define(MON_OR, 'or').
-define(MON_AND, 'and').
-define(MON_REC, rec).
-define(MON_VAR, var).


%% Monitor environment keys.
-define(KEY_ENV, env).
-define(KEY_STR, str).
-define(KEY_VAR, var).
-define(KEY_PAT, pat).

%% Placeholder management.
%%-define(PH_NAMES, [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]).
-define(PH_NAMES, [a]).
-define(PH_PRF, "_@").
-define(KEY_PH_NAMES, ph_names).
-define(KEY_PH_CNT, ph_cnt).

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type line() :: erl_anno:line().
%% Line number in source.

-type with() :: {with, line(), gen_eval:af_mfargs()} |
{with, line(), gen_eval:af_mfargs(), gen_eval:af_constraint()}.
%% Process instrumentation selection MFArgs.

-type spec() :: {spec, line(), with(), af_maxhml()}.
%% Instrumentation specification abstract form.

-type af_maxhml() :: af_hml_tt() | af_hml_ff() | af_hml_pos() | af_hml_nec() |
af_hml_or() | af_hml_and() |
af_hml_max() | af_hml_var().
%% maxHML formulae abstract form.

-type af_hml_ff() :: {ff, line()}.
-type af_hml_tt() :: {tt, line()}.
-type af_hml_pos() :: {pos, line(), gen_eval:af_sym_act(), af_maxhml()}.
-type af_hml_nec() :: {nec, line(), gen_eval:af_sym_act(), af_maxhml()}.
-type af_hml_or() :: {'or', line(), af_maxhml(), af_maxhml()}.
-type af_hml_and() :: {'and', line(), af_maxhml(), af_maxhml()}.
-type af_hml_max() :: {max, line(), af_hml_var(), af_maxhml()}.
-type af_hml_var() :: {var, line(), atom()}.
%% HML formulae abstract form.



%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

compile(File, Opts) ->
  gen_eval:compile(?MODULE, ?LEXER_MOD, ?PARSER_MOD, File, Opts).

parse_string(String) ->
  gen_eval:parse_string(?LEXER_MOD, ?PARSER_MOD, String).

parse_file(File) ->
  gen_eval:parse_file(?LEXER_MOD, ?PARSER_MOD, File).


%%% ----------------------------------------------------------------------------
%%% Private AST manipulation functions.
%%% ----------------------------------------------------------------------------

-spec visit(Node, Opts) -> erl_syntax:syntaxTree()
  when
  Node :: af_maxhml(),
  Opts :: opts:options().
visit(Node = {Bool, _}, _Opts) when Bool =:= ?HML_TRU; Bool =:= ?HML_FLS ->
  ?TRACE("Visiting '~s' node ~p.", [Bool, Node]),

  % Get monitor meta environment for node.
  Env = get_env(Node),
  erl_syntax:tuple([erl_syntax:atom(
    if Bool =:= ?HML_TRU -> ?MON_ACC; Bool =:= ?HML_FLS -> ?MON_REJ end
  ), Env]);

visit(Var = {?HML_VAR, _, _Name}, _Opts) ->
  ?TRACE("Visiting 'var' node ~p.", [Var]),

  % Get monitor meta environment for node.
  Env = get_env(Var),
  erl_syntax:tuple([erl_syntax:atom(?MON_VAR), Env, Var]);

visit(Node = {?HML_MAX, _, Var = {?HML_VAR, _, _}, Phi}, _Opts) ->
  ?TRACE("Visiting 'max' node ~p.", [Node]),

  Clause = erl_syntax:clause(none, [visit(Phi, _Opts)]),
  Fun = erl_syntax:named_fun_expr(Var, [Clause]),

  % Get monitor meta environment for node.
  Env = get_env(Node),
  erl_syntax:tuple([erl_syntax:atom(?MON_REC), Env, Fun]);

visit(Node = {Op, _, Phi, Psi}, _Opts)
  when Op =:= ?HML_OR; Op =:= ?HML_AND ->
  ?TRACE("Visiting '~s' node ~p.", [Op, Node]),

  % Get monitor meta environment for node.
  Env = get_env(Node),
  erl_syntax:tuple(
    [erl_syntax:atom(Op), Env, visit(Phi, _Opts), visit(Psi, _Opts)]
  );

visit(Node = {Mod, _, {act, _, Pat, Guard}, Phi}, _Opts)
  when Mod =:= ?HML_POS; Mod =:= ?HML_NEC ->
  ?TRACE("Visiting '~s' node ~p.", [Mod, Node]),

  % Encode the predicate functions for the action and its inverse. The predicate
  % functions are mutually-exclusive. This means that for any pattern and guard
  % combination, and any value the pattern data variables may be mapped to,
  % these two predicate functions will always return the negated truth value of
  % of each other.
  Pred = erl_syntax:fun_expr([
    erl_syntax:clause([gen_eval:pat_tuple(Pat)], Guard, [erl_syntax:atom(true)]),
    erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(false)])
  ]),

  InvPred = erl_syntax:fun_expr([
    erl_syntax:clause([gen_eval:pat_tuple(Pat)], Guard, [erl_syntax:atom(false)]),
    erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(true)])
  ]),

  % Encode the action bodies. The normal (left) action body consists of the
  % pattern with variables, and the continuation monitor. The inverse (right)
  % action consists of the verdict when the inverse pattern and guard test is
  % successful.
  CntBody = erl_syntax:fun_expr([
    erl_syntax:clause([gen_eval:pat_tuple(Pat)], none, [visit(Phi, _Opts)])
  ]),

  VrdBody = erl_syntax:fun_expr([
    erl_syntax:clause([erl_syntax:underscore()], none, [
      if Mod =:= pos ->
        erl_syntax:tuple([erl_syntax:atom(?MON_REJ), get_env({ff, 0})]);
        Mod =:= nec ->
          erl_syntax:tuple([erl_syntax:atom(?MON_ACC), get_env({tt, 0})])
      end
    ])
  ]),

  % Get a new unique placeholder for this monitor action.
  Ph = new_ph(),

  % Encode left and right action nodes.
  LeftAct = erl_syntax:tuple(
    [erl_syntax:atom(act), get_env(Node, Ph, true), Pred, CntBody]),
  RightAct = erl_syntax:tuple(
    [erl_syntax:atom(act), get_env(Node, Ph, false), InvPred, VrdBody]),

  % Encode the mutually-exclusive choice consisting of the left and right
  % summands.
  erl_syntax:tuple([erl_syntax:atom(chs), get_chs_env(), LeftAct, RightAct]).


%%% ----------------------------------------------------------------------------
%%% Private monitor environment creation functions.
%%% ----------------------------------------------------------------------------

%%% @private Returns an Erlang AST representation of the monitor environment
%%% used to manage the monitor meta information such as the substitution and its
%%% stringified representation.
-spec get_env(Node) -> erl_syntax:syntaxTree()
  when
  Node :: af_hml_tt() | af_hml_ff() | af_hml_or() | af_hml_and() |
  af_hml_max() | af_hml_var().
get_env(Node = {Bool, _}) when Bool =:= ?HML_TRU; Bool =:= ?HML_FLS ->
  Str = new_env_kv(?KEY_STR, get_str(Node)),
  new_env([Str]);
get_env(Node = {Op, _, _, _}) when Op =:= ?HML_OR; Op =:= ?HML_AND ->
  Str = new_env_kv(?KEY_STR, get_str(Node)),
  new_env([Str]);
get_env(Node = {?HML_MAX, _, {?HML_VAR, _, Name}, _}) ->
  Str = new_env_kv(?KEY_STR, get_str(Node)),
  Var = new_env_kv(?KEY_VAR, erl_syntax:atom(Name)),
  new_env([Str, Var]);
get_env(Node = {?HML_VAR, _, Name}) ->
  Str = new_env_kv(?KEY_STR, get_str(Node)),
  Var = new_env_kv(?KEY_VAR, erl_syntax:atom(Name)),
  new_env([Str, Var]).

%%% @private Returns an Erlang AST representation of the monitor environment
%%% for monitor parallel disjunction and conjunction.
-spec get_env(Node, Ph, Inv) -> erl_syntax:syntaxTree()
  when
  Node :: af_hml_pos() | af_hml_nec(),
  Ph :: string(),
  Inv :: boolean().
get_env(Node = {Mod, _, _Act, _Phi}, Ph, Inv)
  when Mod =:= ?HML_POS; Mod =:= ?HML_NEC ->

  % Get stringified representation of the monitor, variable placeholder and
  % pattern used to help stringify the monitor.
  Str = new_env_kv(?KEY_STR, get_str(Node, Ph, Inv)),
  Var = new_env_kv(?KEY_VAR, erl_syntax:atom(Ph)),
  Pat = new_env_kv(?KEY_PAT, get_pat(Node)),
  new_env([Str, Var, Pat]).

%%% @private Returns an Erlang AST representation of the monitor environment for
%%% choice.
-spec get_chs_env() -> erl_syntax:syntaxTree().
get_chs_env() ->
  Str = new_env_kv(?KEY_STR, get_chs_str()),
  new_env([Str]).


%%% @private Returns an Erlang AST representation of a new key-value pair.
-spec new_env_kv(Key, Val) -> erl_syntax:syntaxTree()
  when
  Key :: atom(),
  Val :: erl_syntax:syntaxTree().
new_env_kv(Key, Val) ->
  erl_syntax:tuple([erl_syntax:atom(Key), Val]).

%%% @private Returns an Erlang AST representation of a new monitor environment,
%%% with the specified list elements.
-spec new_env(List :: [erl_syntax:syntaxTree()]) -> erl_syntax:syntaxTree().
new_env(List) ->
  erl_syntax:tuple([erl_syntax:atom(?KEY_ENV), erl_syntax:list(List)]).


%%% ----------------------------------------------------------------------------
%%% Private monitor stringifying and functions.
%%% ----------------------------------------------------------------------------

%%% @private Returns an Erlang ASP representation of the stringified monitor
%%% verdicts, parallel Boolean connectives, and recursion.
-spec get_str(Node) -> erl_syntax:syntaxTree()
  when
  Node :: af_hml_tt() | af_hml_ff() | af_hml_or() | af_hml_and() |
  af_hml_max() | af_hml_var().
get_str({?HML_TRU, _}) ->
  erl_syntax:string("yes");
get_str({?HML_FLS, _}) ->
  erl_syntax:string("no");
get_str({Op, _, _, _}) when Op =:= ?HML_OR; Op =:= ?HML_AND ->
  erl_syntax:string(atom_to_list(Op));
get_str({?HML_MAX, _, {?HML_VAR, _, Name}, _}) ->
  erl_syntax:string(lists:flatten("rec ", atom_to_list(Name)));
get_str({?HML_VAR, _, Name}) ->
  erl_syntax:string(atom_to_list(Name)).

%%% @private Returns an Erlang AST representation of the stringified monitor
%%% actions.
%%%
%%% {@par The action expects a variable placeholder and can generate the action
%%%       or inverse action based on the flag Inv.
%%% }
-spec get_str(Node, Ph, Inv) -> erl_syntax:syntaxTree()
  when
  Node :: af_hml_pos() | af_hml_nec(),
  Ph :: string(),
  Inv :: boolean().
get_str({Mod, _, {?HML_ACT, _, Pat, Guard}, _}, Ph, Inv)
  when Mod =:= ?HML_POS; Mod =:= ?HML_NEC ->

  % Stringify placeholder and the internal representation of the pattern as an
  % Erlang trace event.
  IoList = [Ph, $/, erl_pp:expr(erl_syntax:revert(gen_eval:pat_tuple(Pat)))],

  % Stringify guard only if present.
  IoList_ = if Guard =:= [] -> IoList; true -> [IoList, $ , erl_pp:guard(Guard)] end,

  % Add the stringified negation if the branch is the inverse one (called the)
  % negative branch of mutually-exclusive choice.
  IoList__ = if Inv -> IoList_; true -> ["NOT(", IoList_, ")"] end,

  erl_syntax:string(lists:flatten(IoList__)).

%%% @private Returns an Erlang AST representation of the stringified monitor
%%% mutually-exclusive choice.
-spec get_chs_str() -> erl_syntax:syntaxTree().
get_chs_str() ->
  erl_syntax:string("+").


%%% @private Returns an Erlang AST representation of the native Erlang trace
%%% event patterns with all the variables and 'don't care' patterns replaced by
%%% `undefined'. This is used by the monitoring algorithm to unwrap the monitor
%%% function enclosing monitor actions and compute the stringified
%%% representation of the monitor on the fly.
%%%
%%% {@par The current implementation works, but is inelegant since it piggybacks
%%%       on the Erlang parsing mechanism. The function first converts the
%%%       abstract pattern to an IoList, replaces the variables and 'don't care'
%%%       patterns with `undefined', and parses the result back to an Erlang AST
%%%       representation. The alternative and (perhaps?) more elegant way is to
%%%       implement a replace feature that mutates an Erlang AST. This takes
%%%       time, and must be made to support all the Erlang syntax (unless
%%%       someone else has done it.
%%% }
-spec get_pat(Node :: af_hml_pos() | af_hml_nec()) -> erl_syntax:syntaxTree().
get_pat({Mod, _, {?HML_ACT, _, Pat, Guard}, _})
  when Mod =:= ?HML_POS; Mod =:= ?HML_NEC ->

  Str = erl_pp:expr(erl_syntax:revert(gen_eval:pat_tuple(Pat))),
  Replaced = re:replace(Str, "\\b([A-Z_][a-zA-Z0-9_@]*)\\b", "undefined", [{return, list}, global]),

  {ok, Tokens, _EndLine} = erl_scan:string(Replaced ++ "."),
  {ok, [AbsForm]} = erl_parse:parse_exprs(Tokens),
  AbsForm.


%%% @private Initializes the variable placeholder generator.
-spec init_ph() -> ok.
init_ph() ->

  % Placeholder token list must at least contain one name.
  if length(?PH_NAMES) < 1 -> error("Empty token token names"); true -> ok end,

  put(?KEY_PH_NAMES, ?PH_NAMES), % list of available variable placeholder names.
  put(?KEY_PH_CNT, 0), % 0-based index.
  ok.

%%% @private Checks whether the variable placeholder generator is initialized
%%% and initializes it if not.
-spec check_ph() -> ok.
check_ph() ->
  case get(?KEY_PH_NAMES) of
    undefined ->

      % Placeholder token name generator not initialized.
      init_ph();
    _ ->
      ok
  end.

%%% @private Returns the next unique variable placeholder name.
-spec new_ph() -> string().
new_ph() ->

  % Ensure that placeholder token name generator is initialized.
  check_ph(),

  % Get last placeholder counter and increment it.
  Cnt = put(?KEY_PH_CNT, get(?KEY_PH_CNT) + 1),

  % Get next placeholder token name. Calculation wraps around the counter when
  % the it goes beyond the number of available token names. Access to the list
  % of token names is 1-based.
  Tok = lists:nth((Cnt rem length(?PH_NAMES)) + 1, ?PH_NAMES),

  % Calculate the token name suffix, to generate a unique placeholder token. The
  % suffix is incremented once the counter goes beyond the number of available
  % token names.
  Idx = Cnt div length(?PH_NAMES),

  % Generate unique placeholder name.
  lists:flatten(io_lib:format("~s~s~2..0B", [?PH_PRF, Tok, Idx])).
