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
-module(gen_eval).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-include("log.hrl").

%%% Public API.
-export([create_log/3, pat_tuple/1]).

%%% Callbacks/Internal.
-export([compile/5, parse_string/3, parse_file/3]).

%%% Types.
-export_type([af_sym_act/0, af_mfargs/0, af_constraint/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% File extensions.
-define(EXT_HML, ".hml").
-define(EXT_ERL, ".erl").
-define(EXT_BEAM, ".beam").

%% Monitor entry function.
-define(MFA_SPEC, mfa_spec).

%% Option definitions and their values.
%%-define(OPT_INCLUDE, i). % Kept same option name as Erlang compiler.
-define(OPT_OUT_DIR, outdir). % Kept same option name as Erlang compiler.
-define(OPT_ERL, erl).
-define(OPT_VERBOSE, v).

%% Default Erlang compiler options. These options suppress the variable
%% shadowing warning and returns both compile errors and warnings.
-define(COMPILER_OPTS, [nowarn_shadow_vars, nowarn_unused_vars, return]).

-define(MFARGS, mfargs).

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type line() :: erl_anno:line().
%% Line number in source.

-type error_description() :: term().
%% Internal error description.

-type error_info() :: {line(), module(), error_description()}.
%% Error information.

-type warnings() :: [{file:filename(), [error_info()]}] | [].
%% Warning list.

-type errors() :: [{file:filename(), [error_info()]}] | [].
%% Error list.


-type af_sym_act() :: {act, line(), af_pattern()} |
{act, line(), af_pattern(), af_constraint()}.
%% Symbolic action abstract form.

-type af_pattern() :: af_fork() | af_init() | af_exit() | af_send() | af_recv().
%% Symbolic action pattern abstract form.

-type af_constraint() :: af_guard().
%% Boolean constraint expression abstract form.

-type af_fork() :: {fork, line(), af_variable(), af_variable(), af_mfargs()}.
-type af_init() :: {init, line(), af_variable(), af_variable(), af_mfargs()}.
-type af_exit() :: {exit, line(), af_variable(), abstract_expr()}.
-type af_send() :: {send, line(), af_variable(), af_variable(), abstract_expr()}.
-type af_recv() :: {recv, line(), af_variable(), abstract_expr()}.
%% Abstract process lifecycle and interaction actions.

-type af_mfargs() :: {mfargs, line(), module(), fun_name(), [abstract_expr()]}.
%% Module, function and arguments abstract form.

-type fun_name() :: atom().

%% Monitors.
-type monitor() :: erl_syntax:syntaxTree().


%%% Erlang types.

-type abstract_expr() :: af_literal()
| af_variable()
| af_tuple(abstract_expr())
| af_nil()
| af_cons(abstract_expr())
| af_bin(abstract_expr())
| af_binary_op(abstract_expr())
| af_unary_op(abstract_expr())
| af_map_creation(abstract_expr())
| af_list_comprehension()
| af_binary_comprehension().
%% Expression abstract form.

-type af_list_comprehension() ::
{'lc', line(), af_template(), af_qualifier_seq()}.

-type af_binary_comprehension() ::
{'bc', line(), af_template(), af_qualifier_seq()}.

-type af_template() :: abstract_expr().

-type af_qualifier_seq() :: [af_qualifier(), ...].

-type af_qualifier() :: af_generator() | af_filter().

-type af_generator() :: {'generate', line(), af_pattern(), abstract_expr()}
| {'b_generate', line(), af_pattern(), abstract_expr()}.

-type af_filter() :: abstract_expr().



-type af_map_creation(T) :: {'map', line(), [af_assoc(T)]}.


-type af_assoc(T) :: af_assoc_exact(T).

-type af_assoc_exact(T) :: {'map_field_exact', line(), T, T}.


-type af_guard() :: [af_guard_test(), ...].

-type af_guard_test() :: af_literal()
| af_variable()
| af_tuple(af_guard_test())
| af_nil()
| af_cons(af_guard_test())
| af_bin(af_guard_test())
| af_binary_op(af_guard_test())
| af_unary_op(af_guard_test())
| af_map_creation(af_guard_test()).

-type af_literal() :: af_atom()
| af_character()
| af_float()
| af_integer()
| af_string().

-type af_singleton_integer_type() :: af_integer()
| af_character()
| af_unary_op(af_singleton_integer_type())
| af_binary_op(af_singleton_integer_type()).


-type af_atom() :: af_lit_atom(atom()).

-type af_lit_atom(A) :: {'atom', line(), A}.

-type af_character() :: {'char', line(), char()}.

-type af_float() :: {'float', line(), float()}.

-type af_integer() :: {'integer', line(), non_neg_integer()}.

-type af_string() :: {'string', line(), string()}.

-type af_variable() :: {'var', line(), atom()}. % | af_anon_variable()

-type af_tuple(T) :: {'tuple', line(), [T]}.

-type af_nil() :: {'nil', line()}.

-type af_cons(T) :: {'cons', line(), T, T}.

-type af_bin(T) :: {'bin', line(), [af_binelement(T)]}.

-type af_binelement(T) :: {'bin_element', line(), T, af_binelement_size(), type_specifier_list()}.

-type af_binelement_size() :: 'default' | abstract_expr().

-type af_binary_op(T) :: {'op', line(), binary_op(), T, T}.

-type binary_op() :: '/' | '*' | 'div' | 'rem' | 'band' | 'and' | '+' | '-'
| 'bor' | 'bxor' | 'bsl' | 'bsr' | 'or' | 'xor' | '++'
| '--' | '==' | '/=' | '=<' | '<'  | '>=' | '>' | '=:='
| '=/='.

-type af_unary_op(T) :: {'op', line(), unary_op(), T}.

-type unary_op() :: '+' | '-' | 'bnot' | 'not'.


-type type_specifier_list() :: 'default' | [type_specifier(), ...].

-type type_specifier() :: type()
| signedness()
| endianness()
| unit().

-type type() :: 'integer'
| 'float'
| 'binary'
| 'bytes'
| 'bitstring'
| 'bits'
| 'utf8'
| 'utf16'
| 'utf32'.

-type signedness() :: 'signed' | 'unsigned'.

-type endianness() :: 'big' | 'little' | 'native'.

-type unit() :: {'unit', 1..256}.




%%% ----------------------------------------------------------------------------
%%% Callback definitions.
%%% ----------------------------------------------------------------------------


-callback visit(Node, Opts) -> erl_syntax:syntaxTree()
  when
  Node :: any(),
  Opts :: opts:options().
%% Visits the logic AST and produces the monitor Erlang AST.

%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Compiles the specified file containing one or more properties specified
%% in MaxHML, and writes the executable monitor descriptions to file.
%%
%% {@params
%%   {@name Mod}
%%   {@desc Module implementing the callback function visit/2.}
%%   {@name LexerMod}
%%   {@desc Module that performs the lexical analysis of MaxHML scripts.}
%%   {@name ParserMod}
%%   {@desc Module that parses the lexer tokens to return the MaxHML syntax tree.
%%   }
%%   {@name File}
%%   {@desc Path where the file to compile resides.}
%%   {@name Opts}
%%   {@desc Compiler options.}
%% }
%%
%% {@par The following options are available:
%%       {@dl
%%         {@term `@{outdir, Dir@}'}
%%         {@desc The directory where the generated output monitor file should
%%                be written. Defaults to the current directory `.'.
%%         }
%%         {@term `erl'}
%%         {@desc Instructs the compiler to output the generated monitor as
%%                Erlang source code rather than beam. Defaults to beam.
%%         }
%%       }
%% }
%%
%% {@par Errors and warnings that arise during the compilation are reported on
%%       the standard output. No monitor file is output unless compilation
%%       completes with no errors. The output depends on whether the flag `erl'
%%       is specified or otherwise.
%% }
%%
%% {@returns `ok' if compilation succeeds, `@{error, Reason@}' otherwise.}
compile(Mod, LexerMod, ParserMod, File, Opts) when is_list(Opts) ->

  % Load and parse source script file.
  case parse_file(LexerMod, ParserMod, File) of
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
          write_monitor(create_module(Mod, Ast, ?MFA_SPEC, Module, Opts), File, Opts);

        {error, Reason} ->

          % Error when creating directory.
          erlang:raise(error, Reason, erlang:get_stacktrace())
      end;

    {error, Error} ->

      % Error when performing lexical analysis or parsing.
      show_error(File, Error)
  end.

%% @doc Parses the specified string containing one or more properties specified
%% in MaxHML.
%%
%% {@params
%%   {@name LexerMod}
%%   {@desc Module that performs the lexical analysis of MaxHML scripts.}
%%   {@name ParserMod}
%%   {@desc Module that parses the lexer tokens to return the MaxHML syntax tree.
%%   }
%%   {@name String}
%%   {@desc String to parse.}
%% }
%%
%% {@returns The syntax tree for the properties specified in MaxHML.}
parse_string(LexerMod, ParserMod, String) when is_list(String) ->
  case LexerMod:string(String) of
    {ok, [], _} ->
      {ok, skip};
    {ok, Tokens, _} ->
      case ParserMod:parse(Tokens) of
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

%% @doc Parses the specified file containing one or more properties specified
%% in MaxHML.
%%
%% {@params
%%   {@name LexerMod}
%%   {@desc Module that performs the lexical analysis of MaxHML scripts.}
%%   {@name ParserMod}
%%   {@desc Module that parses the lexer tokens to return the MaxHML syntax tree.
%%   }
%%   {@name File}
%%   {@desc Path where the file to parse resides.}
%% }
%%
%% {@returns The syntax tree for the properties specified in MaxHML.}
parse_file(LexerMod, ParserMod, File) when is_list(File) ->
  case file:read_file(File) of
    {ok, Bytes} ->
      parse_string(LexerMod, ParserMod, binary_to_list(Bytes));
    {error, Reason} ->
      throw({error, {?MODULE, Reason}})
  end.


%% @private Translates the symbolic action patterns fork, init, exit, send and
%% recv to native Erlang trace event patterns.
%%
%% {@par Translation is as follows:
%%   {@ul
%%     {@item Fork `{fork, _, Pid, Pid2, MFArgs}' is translated to
%%            `{trace, Pid, spawn, Pid2, {M, F, Args}}'
%%     }
%%     {@item Initialized `{init, _, Pid2, Pid, MFArgs}' is translated to
%%            `{trace, Pid, spawned, Pid2, {M, F, Args}}'
%%     }
%%     {@item Exit pattern `{exit, _, Pid, Var}' is translated to
%%            `{trace, Pid, exit, Reason}'
%%     }
%%     {@item Send pattern `{send, _, Pid, To, Var}' is translated to
%%            `{trace, Pid, send, Msg, To}'
%%     }
%%     {@item Receive pattern `{recv, _, Pid, Var}' is translated to
%%            `{trace, Pid, 'receive', Msg}'
%%     }
%%   }
%% }
%%-spec pat_tuple(Pattern :: af_pattern()) -> erl_syntax:syntaxTree().
pat_tuple({fork, _, Pid, Pid2, MFArgs}) ->
  erl_syntax:tuple([
    erl_syntax:atom(trace), Pid, erl_syntax:atom(spawn), Pid2,
    mfargs_tuple(MFArgs)]);
pat_tuple({init, _, Pid2, Pid, MFArgs}) ->
  erl_syntax:tuple([
    erl_syntax:atom(trace), Pid2, erl_syntax:atom(spawned), Pid,
    mfargs_tuple(MFArgs)]);
pat_tuple({exit, _, Pid, Var}) ->
  erl_syntax:tuple([
    erl_syntax:atom(trace), Pid, erl_syntax:atom(exit), Var]);
pat_tuple({send, _, Pid, To, Var}) ->
  erl_syntax:tuple([
    erl_syntax:atom(trace), Pid, erl_syntax:atom(send), Var, To]);
pat_tuple({recv, _, Pid, Var}) ->
  erl_syntax:tuple([
    erl_syntax:atom(trace), Pid, erl_syntax:atom('receive'), Var]).

-spec mfargs_tuple(MFArgs :: af_mfargs()) -> erl_syntax:syntaxTree().
mfargs_tuple({?MFARGS, _, M, F, Args}) ->
  erl_syntax:tuple([
    erl_syntax:atom(M), erl_syntax:atom(F), erl_syntax:list(Args)
  ]).


%%% ----------------------------------------------------------------------------
%%% Code generation utility functions.
%%% ----------------------------------------------------------------------------

-spec create_log(Format, Args, Type) -> erl_syntax:syntaxTree()
  when
  Format :: string(),
  Args :: list(),
  Type :: any().
create_log(Format, Args, Type) ->
  Format0 = color_log(["*** [~w] ", Format], Type),
  SelfCall = erl_syntax:application(none, erl_syntax:atom(self), []),
  erl_syntax:application(erl_syntax:atom(io), erl_syntax:atom(format),
    [erl_syntax:string(Format0), erl_syntax:list([SelfCall | Args])]
  ).

%% @private Applies ASCII colors to the specified log message depending on the
%% Type of monitor construct. Type argument determines how the log statement is
%% rendered on the standard output.

-spec color_log(Log, Type) -> any()
  when
  Log :: list(),
  Type :: any().
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
%%% Compiler option functions.
%%% ----------------------------------------------------------------------------

%% @private Configures the Erlang compiler options used to generate the beam
%% code file.

compile_opts(Opts) ->
%%  [{i, include_opt(Opts)}, {i, out_dir_opt(Opts)} | ?COMPILER_OPTS].
  [{i, opts:out_dir_opt(Opts)} | ?COMPILER_OPTS].


%%% ----------------------------------------------------------------------------
%%% Private module bootstrapping functions.
%%% ----------------------------------------------------------------------------

create_module(Mod, Ast, MonFun, Module, Opts) ->

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
    erl_syntax:function(erl_syntax:atom(MonFun), visit_forms(Mod, Ast, Opts))
  ]).

%% @private Visits maxHML formula nodes and generates the corresponding syntax
%% tree describing one monitor (i.e. one formula is mapped to one monitor).
-spec visit_forms(Mod, Form, Opts) -> [erl_syntax:syntaxTree()]
  when
  Mod :: module(),
  Form :: any(),
  Opts :: opts:options().
visit_forms(_Mod, [], Opts) ->

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
visit_forms(Mod, [Form = {form, _, {sel, _, MFArgs = {mfargs, _, M, F, Args}, Guard}, Phi} | Forms], Opts) ->
  ?DEBUG("Form: ~p.", [Form]),
  ?DEBUG("Guard: ~p.", [Guard]),
  ?DEBUG("MFArgs: ~p.", [MFArgs]),



  Body = erl_syntax:tuple([erl_syntax:atom(ok), Mod:visit(Phi, Opts)]),

  [erl_syntax:clause([mfargs_tuple(MFArgs)], Guard, [Body]) | visit_forms(Mod, Forms, Opts)].




%%% ----------------------------------------------------------------------------
%%% Private code generating and compilation functions.
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
%%% Private compilation error handling and reporting functions.
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

%% @private Formats error and warning messages as human-readable descriptions.
%%
%% {@par Result returned is accompanied by the line number where the error
%%       occurred.}
-spec format_error({Line, Mod, Error}) -> {line(), [char() | list()]}
  when
  Line :: line(),
  Mod :: module(),
  Error :: any().
format_error({Line, Mod, Error}) when is_atom(Mod) ->
  {Line, Mod:format_error(Error)}.
%%format_error({Line, maxhml_parser, Error}) ->
%%  {Line, maxhml_parser:format_error(Error)};
%%format_error({Line, maxhml_lexer, Error}) ->
%%  {Line, maxhml_lexer:format_error(Error)}.