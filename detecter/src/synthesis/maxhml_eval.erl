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

-compile(export_all).

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include_lib("syntax_tools/include/merl.hrl").
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

%% File extensions.
-define(EXT_HML, ".hml").
-define(EXT_ERL, ".erl").
-define(EXT_BEAM, ".beam").

-define(MFA_SPEC, mfa_spec).

%% Option definitions and their values.
%%-define(OPT_INCLUDE, i). % Kept same option name as Erlang compiler.
-define(OPT_OUT_DIR, outdir). % Kept same option name as Erlang compiler.
-define(OPT_ERL, erl).
-define(OPT_VERBOSE, v).

%% Default Erlang compiler options.
-define(COMPILER_OPTS, [nowarn_shadow_vars, return]).

%% Monitoring verdicts.
%%-define(ACCEPT, yes).
%%-define(REJECT, no).

%% maxHML logic AST node tags.
-define(LOG_TRU, tt).
-define(LOG_FLS, ff).
-define(LOG_POS, pos).
-define(LOG_NEC, nec).
-define(LOG_OR, 'or').
-define(LOG_AND, 'and').
-define(LOG_MAX, max).
-define(LOG_VAR, var).

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


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

compile(File, Opts) when is_list(Opts) ->

  % Load and parse source script file.
  case parse_file(File) of
    {ok, Ast} ->

      % Before synthesizing monitor as Erlang source or beam code, make ensure
      % the output directory exists.
      case filelib:ensure_dir(util:as_dir_name(opts:out_dir_opt(Opts))) of
        ok ->

          % Extract base name of source script file to create module name. This
          % is used in -module attribute in synthesized monitor module.
          Module = list_to_atom(filename:basename(File, ?EXT_HML)),

          % Synthesize monitor from parsed syntax tree in the form of an Erlang
          % syntax tree and write result to file as Erlang source or beam code.
          write_monitor(create_module(Ast, ?MFA_SPEC, Module, Opts), File, Opts);

        {error, Reason} ->

          % Error when creating directory.
          erlang:raise(error, Reason, erlang:get_stacktrace())
      end;

    {error, Error} ->

      % Error when performing lexical analysis or parsing.
      show_error(File, Error)
  end.

parse_string(String) when is_list(String) ->
  case maxhml_lexer:string(String) of
    {ok, [], _} ->
      {ok, skip};
    {ok, Tokens, _} ->
      case maxhml_parser:parse(Tokens) of
        {ok, Ast} ->
          {ok, Ast};
        {error, Error = {_, _, _}} ->

          % Error in parsing.
          {error, Error}
      end;
    {error, Error = {_, _, _}, _} ->

      % Error in lexical analysis.
      {error, Error}
  end.


parse_file(File) when is_list(File) ->
  case file:read_file(File) of
    {ok, Bytes} ->
      parse_string(binary_to_list(Bytes));
    {error, Reason} ->
      throw({error, {?MODULE, Reason}})
  end.

%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Compiler option functions.
%%% ----------------------------------------------------------------------------

%% @private Configures the Erlang compiler options used to generate the beam
%% code file.

compile_opts(Opts) ->
%%  [{i, include_opt(Opts)}, {i, out_dir_opt(Opts)} | ?COMPILER_OPTS].
  [{i, opts:out_dir_opt(Opts)} | ?COMPILER_OPTS].


%%% ----------------------------------------------------------------------------
%%% Private AST manipulation functions.
%%% ----------------------------------------------------------------------------

% TODO: Implement check for unguarded variables

create_module(Ast, MonFun, Module, Opts) ->

  % Create monitor file meta information.
  {{YY, MM, DD}, {HH, Mm, SS}} = calendar:local_time(),
  Date = io_lib:format("~4B/~2B/~2..0B ~2..0B:~2..0B:~2..0B",
    [YY, MM, DD, HH, Mm, SS]),

  % Generate module base and attribute meta information.
  Forms = ?Q([
    "-module('@Module@').",
    "-author(\"detectEr\").",
    "-generated('@Date@').",
    "-export(['@MonFun@'/1])."
  ]),

  % Create monitor module.
  erl_syntax:revert_forms(Forms ++ [
    erl_syntax:function(erl_syntax:atom(MonFun), visit_forms(Ast, Opts))
  ]).

%% @private Visits maxHML formula nodes and generates the corresponding syntax
%% tree describing one monitor (i.e. one formula is mapped to one monitor).

visit_forms([], Opts) ->

  % Generate catchall function clause pattern that matches Mod:Fun(Args) pattern
  % to return undefined. This is the case when no monitor should be attached to
  % said MFA.
  case opts:verbose_opt(Opts) of
    true ->

      % Create verbose function clause body to include logging information.
      MfaVar = erl_syntax:variable('_Mfa'),
      Log = create_log("Skipping instrumentation for MFA pattern '~p'.~n", [MfaVar], no),
      [erl_syntax:clause([MfaVar], none, [Log | [erl_syntax:atom(undefined)]])];
    _ ->
      [erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(undefined)])]
  end;
%%visit_forms([{form, _, {mfa, _, Mod, Fun, _, Clause}, Shml} | Forms], Opts) ->
%%visit_forms([Form | Forms], Opts) ->
visit_forms([Form = {form, _, {sel, _, MFArgs = {mfargs, _, M, F, Args}, Guard}, Phi} | Forms], Opts) ->
  ?DEBUG("Form: ~p.", [Form]),
  ?DEBUG("Guard: ~p.", [Guard]),
  ?DEBUG("MFArgs: ~p.", [MFArgs]),


%%  Body = erl_syntax:atom(undefined), % This needs to be obnained from visitmaxhml

  Body = erl_syntax:tuple([erl_syntax:atom(ok), visit_node(Phi, Opts)]),

  [erl_syntax:clause([mfargs_tuple(MFArgs)], Guard, [Body]) | visit_forms(Forms, Opts)].


%%-spec visit_node(Node, Opts) -> erl_syntax:syntaxTree() | [erl_syntax:syntaxTree()].

visit_node(Node = {Bool, _}, _Opts) when Bool =:= ?LOG_TRU; Bool =:= ?LOG_FLS ->
  ?TRACE("Visiting '~s' node ~p.", [Bool, Node]),

  % Get monitor meta environment for node.
  Env = get_env(Node),
  erl_syntax:tuple([erl_syntax:atom(
    if Bool =:= ?LOG_TRU -> ?MON_ACC; Bool =:= ?LOG_FLS -> ?MON_REJ end
  ), Env]);

visit_node(Var = {?LOG_VAR, _, _Name}, _Opts) ->
  ?TRACE("Visiting 'var' node ~p.", [Var]),

  % Get monitor meta environment for node.
  Env = get_env(Var),
  erl_syntax:tuple([erl_syntax:atom(?MON_VAR), Env, Var]);

visit_node(Node = {?LOG_MAX, _, Var = {?LOG_VAR, _, _}, Phi}, _Opts) ->
  ?TRACE("Visiting 'max' node ~p.", [Node]),

  Clause = erl_syntax:clause(none, [visit_node(Phi, _Opts)]),
  Fun = erl_syntax:named_fun_expr(Var, [Clause]),

  % Get monitor meta environment for node.
  Env = get_env(Node),
  erl_syntax:tuple([erl_syntax:atom(?MON_REC), Env, Fun]);

visit_node(Node = {Op, _, Phi, Psi}, _Opts)
  when Op =:= ?LOG_OR; Op =:= ?LOG_AND ->
  ?TRACE("Visiting '~s' node ~p.", [Op, Node]),

  % Get monitor meta environment for node.
  Env = get_env(Node),
  erl_syntax:tuple(
    [erl_syntax:atom(Op), Env, visit_node(Phi, _Opts), visit_node(Psi, _Opts)]
  );

visit_node(Node = {Mod, _, {act, _, Pat, Guard}, Phi}, _Opts)
  when Mod =:= ?LOG_POS; Mod =:= ?LOG_NEC ->
  ?TRACE("Visiting '~s' node ~p.", [Mod, Node]),

  % Encode the predicate functions for the action and its inverse. The predicate
  % functions are mutually-exclusive. This means that for any pattern and guard
  % combination, and any value the pattern data variables may be mapped to,
  % these two predicate functions will always return the negated truth value of
  % of each other.
  Pred = erl_syntax:fun_expr([
    erl_syntax:clause([pat_tuple(Pat)], Guard, [erl_syntax:atom(true)]),
    erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(false)])
  ]),

  InvPred = erl_syntax:fun_expr([
    erl_syntax:clause([pat_tuple(Pat)], Guard, [erl_syntax:atom(false)]),
    erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(true)])
  ]),

  % Encode the action bodies. The normal (left) action body consists of the
  % pattern with variables, and the continuation monitor. The inverse (right)
  % action consists of the verdict when the inverse pattern and guard test is
  % successful.
  CntBody = erl_syntax:fun_expr([
    erl_syntax:clause([pat_tuple(Pat)], none, [visit_node(Phi, _Opts)])
  ]),

  VrdBody = erl_syntax:fun_expr([
    erl_syntax:clause([erl_syntax:underscore()], none, [
      if Mod =:= pos ->
        erl_syntax:tuple([erl_syntax:atom(?MON_REJ), get_env({ff, -1})]);
        Mod =:= nec ->
          erl_syntax:tuple([erl_syntax:atom(?MON_ACC), get_env({tt, -1})])
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


% Name: atom
% Env : syntaxTree
% Act : syntaxTree


% Contains the meta monitor logic for managing the environment.
get_env(Node = {tt, _}) ->
  Str = new_env_kv(?KEY_STR, get_str(Node)),
  new_env([Str]);
get_env(Node = {ff, _}) ->
  Str = new_env_kv(?KEY_STR, get_str(Node)),
  new_env([Str]);
get_env(Node = {Op, _, _, _}) when Op =:= 'or'; Op =:= 'and' ->
  Str = new_env_kv(?KEY_STR, get_str(Node)),
  new_env([Str]);
get_env(Node = {max, _, {var, _, Name}, _}) ->
  Str = new_env_kv(?KEY_STR, get_str(Node)),
  Var = new_env_kv(?KEY_VAR, erl_syntax:atom(Name)),
  new_env([Str, Var]);
get_env(Node = {var, _, Name}) ->
  Str = new_env_kv(?KEY_STR, get_str(Node)),
  Var = new_env_kv(?KEY_VAR, erl_syntax:atom(Name)),
  new_env([Str, Var]).





get_env(Node = {Mod, _, Act, Phi}, Ph, Neg) when Mod =:= pos; Mod =:= nec ->


  Str = new_env_kv(?KEY_STR, get_str(Node, Ph, Neg)),
  Var = new_env_kv(?KEY_VAR, erl_syntax:atom(Ph)),
  Pat = new_env_kv(?KEY_PAT, get_pat(Node)),
  ?INFO("PAT is: ~p", [Pat]),

  new_env([Str, Var, Pat]).

get_chs_env() ->
  Str = new_env_kv(?KEY_STR, get_chs_str()),
  new_env([Str]).


%% TODO: Add the cases for the nec and pos to generate their own environment.


% key : atom
% val : syntaxTree

%%% @private Returns an Erlang AST representation of a new key-value pair.
-spec new_env_kv(Key, Val) -> erl_syntax:syntaxTree()
  when
  Key :: atom(),
  Val :: erl_syntax:syntaxTree().
new_env_kv(Key, Val) ->
  erl_syntax:tuple([erl_syntax:atom(Key), Val]).

%%% @private Returns an Erlang AST representation of a new monitor environment,
%%% with the specified list elements.
-spec new_env(List :: list(erl_syntax:syntaxTree())) -> erl_syntax:syntaxTree().
new_env(List) ->
  erl_syntax:tuple([erl_syntax:atom(?KEY_ENV), erl_syntax:list(List)]).

%%% @private Returns an Erlang ASP representation of the stringified monitor.
%% TODO: Add a proper type later.
-spec get_str(any()) -> erl_syntax:syntaxTree().
get_str({tt, _}) ->
  erl_syntax:string("yes");
get_str({ff, _}) ->
  erl_syntax:string("no");
get_str({Op, _, _, _}) when Op =:= 'or'; Op =:= 'and' ->
  erl_syntax:string(atom_to_list(Op));
get_str({max, _, {var, _, Name}, _}) ->
  erl_syntax:string(["rec ", atom_to_list(Name)]);
get_str({var, _, Name}) ->
  erl_syntax:string(atom_to_list(Name)).


get_str({Mod, _, {act, _, Pat, Guard}, _}, Ph, Neg) when Mod =:= pos; Mod =:= nec ->

  % Stringify placeholder and the internal representation of the pattern as an
  % Erlang trace event.
  IoList = [Ph, $/, erl_pp:expr(erl_syntax:revert(pat_tuple(Pat)))],

  % Stringify guard only if present.
  IoList_ = if Guard =:= [] -> IoList; true -> [IoList, $ , erl_pp:guard(Guard)] end,

  % Add the stringified negation if the branch is the inverse one (called the)
  % negative branch of mutually-exclusive choice.
  IoList__ = if Neg -> IoList_; true -> ["NOT(", IoList_, ")"] end,

  erl_syntax:string(IoList__).

get_chs_str() ->
  erl_syntax:string("+").




get_pat({Mod, _, {act, _, Pat, Guard}, _}) when Mod =:= pos; Mod =:= nec ->

  Str = erl_pp:expr(erl_syntax:revert(pat_tuple(Pat))),


%%  Replaced = re:replace(Str, "\W_\W", "undefined", [{return, list}, global]),
  Replaced = re:replace(Str, "\\b([A-Z_][a-zA-Z0-9_@]*)\\b", "undefined", [{return, list}, global]),

  ?INFO("The Originial patternn is: ~s", [Str]),
  ?INFO("The replaced patternn is: ~s", [Replaced]),

  {ok, Tokens, _EndLine} = erl_scan:string(Replaced ++ "."),
  {ok, [AbsForm]} = erl_parse:parse_exprs(Tokens),

%%  P0 = pat_tuple(Pat).

%%  \b(_[a-zA-Z0-9_@]*)|([A-Z][a-zA-Z0-9_@]*)\b

  AbsForm.



init_ph() ->

  % Placeholder token list must at least contain one name.
  if length(?PH_NAMES) < 1 -> error("Empty token token names"); true -> ok end,

  put(?KEY_PH_NAMES, ?PH_NAMES), %
  put(?KEY_PH_CNT, 0). % 0-based index.

check_ph() ->

  case get(?KEY_PH_NAMES) of
    undefined ->

      % Placeholder token name generator not initialized.
      init_ph();
    _ ->
      ok
  end.

% Generates the next variable place holder.
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
%%  list_to_atom(lists:flatten(io_lib:format("~s~s~2..0B", [?PH_PRF, Tok, Idx]))).
  lists:flatten(io_lib:format("~s~s~2..0B", [?PH_PRF, Tok, Idx])).


%%act_clauses({act, _, Pat, Guard}, ActBody, InvBody) ->
%%  [erl_syntax:clause([pat_tuple(Pat)], Guard, ActBody),
%%    erl_syntax:clause([erl_syntax:underscore()], none, InvBody)].










pat_tuple({send, _, Pid, To, Var}) ->
%%  {trace, Pid, send, Msg, To}
  erl_syntax:tuple([
    erl_syntax:atom(trace), Pid, erl_syntax:atom(send), Var, To]);
pat_tuple({recv, _, Pid, Var}) ->
%%  {trace, Pid, 'receive', Msg}
  erl_syntax:tuple([
    erl_syntax:atom(trace), Pid, erl_syntax:atom('receive'), Var]);
pat_tuple({fork, _, Pid, Pid2, MFArgs}) ->
%%  {trace, Pid, spawn, Pid2, {M, F, Args}}
  erl_syntax:tuple([
    erl_syntax:atom(trace), Pid, erl_syntax:atom(spawn), Pid2,
    mfargs_tuple(MFArgs)]);

pat_tuple({init, _, Pid2, Pid, MFArgs}) ->
%%  {trace, Pid, spawned, Pid2, {M, F, Args}}
  erl_syntax:tuple([
    erl_syntax:atom(trace), Pid2, erl_syntax:atom(spawned), Pid,
    mfargs_tuple(MFArgs)]);

pat_tuple({exit, _, Pid, Var}) ->
%%  {trace, Pid, exit, Reason}
  erl_syntax:tuple([
    erl_syntax:atom(trace), Pid, erl_syntax:atom(exit), Var]).


% TODO: Check whether this is used more than once. Yes it is!!
mfargs_tuple({mfargs, _, M, F, Args}) ->
  erl_syntax:tuple([
    erl_syntax:atom(M), erl_syntax:atom(F), erl_syntax:list(Args)
  ]).





create_log(Format, Args, Type) ->
  Format0 = color_log(["*** [~w] ", Format], Type),
  SelfCall = erl_syntax:application(none, erl_syntax:atom(self), []),
  erl_syntax:application(erl_syntax:atom(io), erl_syntax:atom(format),
    [erl_syntax:string(Format0), erl_syntax:list([SelfCall | Args])]
  ).

%% @private Applies ASCII colors to the specified log message depending on the
%% Type of monitor construct. Type argument determines how the log statement is
%% rendered on the standard output.

color_log(Log, no) ->
  lists:flatten(["\e[1m\e[31m", Log, "\e[0m"]); % Bold red.
color_log(Log, prf) ->
  lists:flatten(["\e[37m", Log, "\e[0m"]); % White.
color_log(Log, var) ->
  lists:flatten(["\e[36m", Log, "\e[0m"]); % Cyan.
color_log(Log, 'end') ->
  lists:flatten(["\e[1m\e[33m", Log, "\e[0m"]); % Bold yellow.
color_log(Log, _) ->
  lists:flatten(Log).


%%% ----------------------------------------------------------------------------
%%% Private code generating functions.
%%% ----------------------------------------------------------------------------

write_monitor(Ast, File, Opts) ->

  % Create base filename, taking into account the output directory specified in
  % the compiler options.
  FileBase = filename:join([opts:out_dir_opt(Opts), filename:basename(File, ?EXT_HML)]),

  % Open file for writing and write Erlang source or beam code depending on
  % the specified compiler options.
  % Open file for writing. File extension depends on specified compiler options.
  {ok, IoDev} = file:open(FileBase ++
  case opts:erl_opt(Opts) of true -> ?EXT_ERL; _ -> ?EXT_BEAM end, [write]
  ),

  % Write monitor Erlang or beam source code depending on specified compiler
  % options.
  case opts:erl_opt(Opts) of
    true ->
      write_erl(IoDev, Ast, File, compile_opts(Opts));
    _ ->
      write_beam(IoDev, Ast, File, compile_opts(Opts))
  end,

  % Close file.
  file:close(IoDev).


write_erl(IoDev, Ast, File, CompileOpts) ->

  % Lint Erlang syntax tree and report any errors or warnings found to standard
  % output. If linting completes without errors, write sources code into a .erl
  % file.
  case erl_lint:module(Ast, File, CompileOpts) of
    Ok = {ok, Warnings} ->
      show_warnings(Warnings),
      list_erl(IoDev, Ast),
      Ok;
    Error = {error, Errors, Warnings} ->
      show_errors(Errors),
      show_warnings(Warnings),
      Error
  end.

write_beam(IoDev, Ast, File, CompileOpts) ->

  % Compile Erlang syntax tree and report any errors or warnings found to
  % standard output. If compilation completes without errors, write object code
  % into a .beam file.
  case compile:forms(Ast, [{source, File} | CompileOpts]) of
    Ok = {ok, _, Binary, Warnings} ->
      show_warnings(Warnings),
      list_beam(IoDev, Binary),
      Ok;
    Error = {error, Errors, Warnings} ->
      show_errors(Errors),
      show_warnings(Warnings),
      Error
  end.


list_erl(_, []) ->
  ok;
list_erl(IoDev, [Form | Forms]) ->
  io:put_chars(IoDev, erl_pp:form(Form)),
  list_erl(IoDev, Forms).

%% @private Writes the binary as it to the specified IO device.

list_beam(IoDev, Beam) ->
  file:write(IoDev, Beam).


%%% ----------------------------------------------------------------------------
%%% Private error handling and reporting functions.
%%% ----------------------------------------------------------------------------

show_error(File, Error) ->
  {Line, Desc} = format_error(Error),
  io:format("~s:~b: ~s~n", [File, Line, Desc]).

show_errors([]) ->
  ok;
show_errors([{File, ErrorsInfos}]) ->
  lists:map(fun(ErrorInfo) -> show_error(File, ErrorInfo) end, ErrorsInfos),
  ok.
%%show_errors(_) ->
%%  ok.

show_warnings([]) ->
  ok;
show_warnings([{File, ErrorInfos}]) ->
  lists:map(
    fun(ErrorInfo) ->
      {Line, Desc} = format_error(ErrorInfo),
      io:format("~s:~b: Warning: ~s~n", [File, Line, Desc])
    end, ErrorInfos),
  ok.
%%;
%%show_warnings(_) ->
%%  ok.

%% @private Formats error and warning messages as human-readable descriptions,
%% returning the results together with the corresponding line number where the
%% error occurred.

format_error({Line, erl_lint, Error}) ->
  {Line, erl_lint:format_error(Error)};
format_error({Line, maxhml_parser, Error}) ->
  {Line, maxhml_parser:format_error(Error)};
format_error({Line, maxhml_lexer, Error}) ->
  {Line, maxhml_lexer:format_error(Error)}.