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
-define(ACCEPT, yes).
-define(REJECT, no).



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

%% @private Visits SHMLnf formula nodes and generates the corresponding syntax
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
visit_node(_Node = {tt, _}, Opts) ->
  ?TRACE("Visiting 'tt' node ~p.", [_Node]),
  erl_syntax:atom(yes);

visit_node(_Node = {ff, _}, Opts) ->
  ?TRACE("Visiting 'ff' node ~p.", [_Node]),
  erl_syntax:atom(no);

visit_node(Var = {var, _, Name}, Opts) ->
  ?TRACE("Visiting 'var' node ~p.", [Var]),
  erl_syntax:application(Var, []);

visit_node(_Node = {max, _, Var = {var, _, _}, Phi}, Opts) ->
  ?TRACE("Visiting 'max' node ~p.", [_Node]),

  Clause = erl_syntax:clause(none, [visit_node(Phi, Opts)]),

  erl_syntax:application(erl_syntax:named_fun_expr(Var, [Clause]), []);

visit_node(_Node = {'or', _, Phi, Psi}, Opts) ->
  ?TRACE("Visiting 'or' node ~p.", [_Node]),

  


  ok;

visit_node(_Node = {'and', _, Phi, Psi}, Opts) ->
  ?TRACE("Visiting 'and' node ~p.", [_Node]),
  ok;

visit_node(_Node = {pos, _, Act, Phi}, Opts) ->
  ?TRACE("Visiting 'pos' node ~p.", [_Node]),

  erl_syntax:fun_expr(
    act_clauses(Act, [visit_node(Phi, Opts)], [erl_syntax:atom(?REJECT)]));


visit_node(_Node = {nec, _, Act, Phi}, Opts) ->
  ?TRACE("Visiting 'nec' node ~p.", [_Node]),




  % TODO: We need two extra clauses: the usual catchall _ for end, and the
  % complement of the boolean condition that for nec, would be yes, for pos
  % would be no.

  % TODO: I think that, since the mutually exclusive choice + is implemented as
  % TODO: guards, we can simply use the _ pattern to generate yes or no's
  % TODO: depending on whether a pos or a nec is being processed. But would this
  % TODO: mean that there will be no place for the 'end' verdict?
  % TODO: Question: What happens when we have a variable/pattern that matches
  % TODO: anything. What its complement set of action. Is it the empty set of
  % TODO: actions (since we have potentially an infinite set that matches)?


  erl_syntax:fun_expr(
    act_clauses(Act, [visit_node(Phi, Opts)], [erl_syntax:atom(?ACCEPT)])).





act_clauses({act, _, Pat, Guard}, ActBody, InvBody) ->
  [erl_syntax:clause([pat_tuple(Pat)], Guard, ActBody),
    erl_syntax:clause([erl_syntax:underscore()], none, InvBody)].









pat_tuple({send, _, Pid, To, Var}) ->
%%  {trace, Pid, send, Msg, To}
  erl_syntax:tuple([
    erl_syntax:atom(trace), Pid, erl_syntax:atom(send), To, Var]);
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