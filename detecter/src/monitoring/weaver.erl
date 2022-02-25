%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Module description (becomes module heading).
%%%
%%% @end
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
-module(weaver).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([weave/3, weave_file/3]).

%%% Callbacks/Internal.
-export([parse_transform/2]).

%%% Types.
-export_type([comp_ret/0, filter/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------
-define(MONITOR, '$monitor').

-define(EXT_REGEX, "^.*\\.erl$").

-define(TRACE_MOD, analyzer).
-define(TRACE_FUN, dispatch).

-define(OPT_MFA_SPEC, mfa_spec).
-define(OPT_FILTER, filter).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type option() :: opts:option() | {filter, function()}.
%% Options passed to the parse_transform function to control the weaving of
%% tracing and monitor.

-type options() :: [option()].
%% Compiler option list.

-type filter() :: fun((Pattern :: term()) -> boolean).
%% Filter used to suppress trace events from being generated and sent to the
%% monitor for analysis.

-type err_info() :: {erl_anno:line() | none, module(), term()}.
%% Error or warning details identifying the source line number, module raising
%% the error and term describing the error or warning in question.

-type errors() :: [{file:filename(), [err_info()]}].
%% Errors in file.

-type warnings() :: [{file:filename(), [err_info()]}].
%% Warnings in file.

-type mod_ret() :: {ok, module()} | {'ok', module(), warnings()}.
%% Successful compilation result of module.

-type err_ret() :: error | {error, errors(), warnings()}.
%% Unsuccessful compilation result of module consisting of errors and warnings.

-type comp_ret() :: mod_ret() | err_ret().
%% Compilation result.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

% TODO: Refactor with erl_parse module. Some other time.

%% @doc Instruments all modules in the specified directory by weaving in tracing
%% and monitor instructions through a transformation of the corresponding module
%% abstract syntax tree.
%%
%% {@params
%%   {@name SrcDir}
%%   {@desc The directory path containing the source or object files to be
%%          inspected and weaved.
%%   }
%%   {@name MfaSpec}
%%   {@desc The function that specifies which remote (<i>i.e.,</i> external)
%%          function calls are to be weaved with tracing and monitor
%%          instructions.
%%   }
%% }
%%
%% {@par Object (`.beam') files that require instrumentation need to be compiled
%%       in debug mode.
%% }
%%
%% {@par The following options are available:
%%       {@dl
%%         {@term `@{outdir, Dir@}'}
%%         {@desc The directory where the generated weaved files should
%%                be written. Defaults to the current directory `.'.
%%         }
%%         {@term `@{i, Dir@}'}
%%         {@desc The directory containing include files that the source files
%%                in `SrcDir' depend on.
%%         }
%%         {@term `@{filer, Fun@}'}
%%         {@desc The filter function that suppressed events. Defaults to allows
%%                all.
%%         }
%%         {@term `erl'}
%%         {@desc Instructs the compiler to output the generated files as
%%                Erlang source code rather than beam. Defaults to beam.
%%         }
%%       }
%% }
%%
%% {@returns A list containing the compilation result of each file in `SrcDir'.
%%           Warnings are included if present.
%% }
-spec weave(SrcDir, MfaSpec, Opts) -> [comp_ret()]
  when
  SrcDir :: string(),
  MfaSpec :: analyzer:mfa_spec(),
  Opts :: options().
weave(SrcDir, MfaSpec, Opts) when is_function(MfaSpec, 1) ->
  case filelib:ensure_dir(util:as_dir_name(opts:out_dir_opt(Opts))) of
    ok ->

      % Recursively obtain list of source files and apply AST transformation.
      Files = filelib:fold_files(
        SrcDir, ?EXT_REGEX, true, fun(F, Acc) -> [F | Acc] end, []),
      Compiled = [write_file(File, MfaSpec, Opts) || File <- Files],

      % Load successfully compiled modules in code path.
      load_mods(compiled_mods(Compiled)),
      Compiled;

    {error, Reason} ->
      erlang:raise(error, Reason, erlang:get_stacktrace())
  end.

%% @doc Instruments the specified module file by weaving in tracing and monitor
%% instructions through a transformation of the corresponding module abstract
%% syntax tree.
%%
%% {@params
%%   {@name SrcFile}
%%   {@desc The file to be inspected and weaved.
%%   }
%%   {@name MfaSpec}
%%   {@desc The function that specifies which remote (<i>i.e.,</i> external)
%%          function calls are to be weaved with tracing and monitor
%%          instructions.
%%   }
%% }
%%
%% {@par Object (`.beam') files that require instrumentation need to be compiled
%%       in debug mode.
%% }
%%
%% {@par See {@link weave/3} for configuration options.}
%%
%% {@returns A list containing the compilation result of each file in `SrcDir'.
%%           Warnings are included if present.
%% }
-spec weave_file(SrcFile, MfaSpec, Opts) -> comp_ret()
  when
  SrcFile :: string(),
  MfaSpec :: analyzer:mfa_spec(),
  Opts :: options().
weave_file(File, MfaSpec, Opts) when is_function(MfaSpec, 1) ->
  case filelib:ensure_dir(util:as_dir_name(opts:out_dir_opt(Opts))) of
    ok ->

      % Apply AST transformation.
      Compiled = write_file(File, MfaSpec, Opts),

      % Load compiled module in code path if successful.
      load_mods(compiled_mods([Compiled])),
      Compiled;

    {error, Reason} ->
      erlang:raise(error, Reason, erlang:get_stacktrace())
  end.


%%% ----------------------------------------------------------------------------
%%% Compiler option functions.
%%% ----------------------------------------------------------------------------

%% @private Returns the filter function option if defined. Defaults to
%% `fun(_) -> true end' which does not suppress any events.
filter_opt(Opts) ->
  proplists:get_value(?OPT_FILTER, Opts, fun analyzer:filter/1).


%%% ----------------------------------------------------------------------------
%%% Callbacks.
%%% ----------------------------------------------------------------------------

%% @private Applies transformations on the abstract syntax to weave in tracing and
%% monitor instructions.
-spec parse_transform(Ast, Opts) -> Ast0 :: [erl_parse:abstract_form()]
  when
  Ast :: [erl_parse:abstract_form()],
  Opts :: [option()].
parse_transform(Ast, Opts) ->
  MfaSpec = proplists:get_value(?OPT_MFA_SPEC, Opts),
  Filter = proplists:get_value(?OPT_FILTER, Opts),
  {_, Ast0} = trans_ast(Ast, MfaSpec, Filter, 0),
  Ast0.


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% @private Weaves monitor by applying AST transformation and writes the result
%% to file.
%%
%% Written file depends on whether the 'erl' option is set. If 'erl' is set,
%% file is written as Erlang source code, otherwise object code is written.
%%
%% If output directory is specified, it is assumed to exist.
write_file(File, MfaSpec, Opts) ->

  % Set up default compiler options.
  CompileOpts = [
    {i, opts:include_opt(Opts)}, {outdir, opts:out_dir_opt(Opts)}, return,
    {?OPT_MFA_SPEC, MfaSpec}, {?OPT_FILTER, filter_opt(Opts)},
    {parse_transform, ?MODULE}, {d, 'WEAVED'}
  ],

  % Append 'E' flag to compiler to generate source code instead of beam.
  CompileOpts0 =
    case opts:erl_opt(Opts) of
      true -> CompileOpts ++ ['E']; _ -> CompileOpts
    end,

  % Apply AST transformation, weaving in monitor code.
  Ret = compile:file(File, CompileOpts0),

  % If 'E' flag was included in compiler options, the resulting weaved file
  % is output as source code. The 'E' cannot be controlled from compiler
  % options. We rename the file ourselves to .erl so that the weaver output
  % is standard.
  case opts:erl_opt(Opts) of
    true ->
      Ext = opts:file_ext(Opts),
      FileBase = filename:basename(File, Ext),
      file:rename(
        filename:join(opts:out_dir_opt(Opts), FileBase ++ ".E"),
        filename:join(opts:out_dir_opt(Opts), FileBase ++ Ext)
      );
    _ ->
      ok
  end,

  % Return compiler result.
  Ret.

%%load_module(Mod) ->
%%  ok.

load_mods(Mods) ->
  [begin
     if length(M) > 0 -> code:purge(M), code:load_file(M); true -> ok end
   end || M <- Mods].

compiled_mods(Compiled) ->
  lists:filtermap(fun({ok, Mod, _}) -> {true, Mod}; (_) -> false end, Compiled).

%% @private Transforms a list of forms. Only functions are currently handled;
%% any other form is left as is.
-spec trans_ast(Ast, MfaSpec, FilterSpec, Id) ->
  {Id0 :: non_neg_integer(), Ast0 :: [erl_parse:abstract_form()]}
  when
  Ast :: [erl_parse:abstract_form()],
  MfaSpec :: analyzer:mfa_spec(),
  FilterSpec :: filter(),
  Id :: non_neg_integer().
trans_ast([], _, _, Id) ->
  {Id, []};
trans_ast([Form | Forms], MfaSpec, FilterSpec, Id) ->
  {Id0, Form0} = trans_form(Form, MfaSpec, FilterSpec, Id),
  {Id1, Forms0} = trans_ast(Forms, MfaSpec, FilterSpec, Id0),
  {Id1, [Form0 | Forms0]}.

%% @private Transforms a form. Only functions are currently handled; any other
%% form is left as is.
-spec trans_form(Form, MfaSpec, FilterSpec, Id) ->
  {Id0 :: non_neg_integer(), Form0 :: erl_parse:abstract_form()}
  when
  Form :: erl_parse:abstract_form(),
  MfaSpec :: analyzer:mfa_spec(),
  FilterSpec :: filter(),
  Id :: non_neg_integer().
trans_form({function, Line, Name, Arity, Clauses}, MfaSpec, FilterSpec, Id) ->
  {Id0, Clauses0} = trans_clauses(Clauses, MfaSpec, FilterSpec, Id),
  {Id0, {function, Line, Name, Arity, Clauses0}};
trans_form(Form, _, _, Id) ->
  {Id, Form}.


% Body as a list of expressions.
%% @private Transforms the function body (which is a list of expressions).
-spec trans_body(Body, MfaSpec, FilterSpec, Id) ->
  {Id0 :: non_neg_integer(), [erl_parse:abstract_expr()]}
  when
  Body :: [erl_parse:abstract_expr()],
  MfaSpec :: analyzer:mfa_spec(),
  FilterSpec :: filter(),
  Id :: non_neg_integer().
trans_body([], _, _, Id) ->
  {Id, []};
trans_body([Expr | Body], MfaSpec, FilterSpec, Id) ->
  {Id0, Expr0} = trans_expr(Expr, MfaSpec, FilterSpec, Id),
  {Id1, Body0} = trans_body(Body, MfaSpec, FilterSpec, Id0),
  {Id1, [Expr0 | Body0]}.

%% @private Transforms expression to weave in tracing and monitoring
%% instructions for spawn, send and receive. The following expressions are
%% handled:
%% {@ul
%% {@item blocks `block'}
%% {@item pattern matches and variable assignments `match'}
%% {@item unary operators `op'}
%% {@item binary operators `op' including `!'}
%% {@item tuples `tuple'}
%% {@item lists `cons' and `nil'}
%% {@item bitstring comprehensions `bc' and construction `bin'}
%% {@item map creation and update `map'}
%% {@item record update and record with expression `record', field access
%%        `record_field', indexing `record_index'
%% }
%% {@item cases `case'}
%% {@item try `try' and catch `catch'}
%% {@item anonymous `fun' and named anonymous `named_fun' function}
%% {@item external function calls `remote' including `spawn' and `spawn_link'}
%% {@item internal function calls `call' including `spawn' and `spawn_link'}
%% {@item if conditions `if'}
%% {@item list comprehensions `lc'}
%% {@item receives `receive'}
%% {@item all other expressions}
%% }
-spec trans_expr(Expr, MfaSpec, FilterSpec, Id) ->
  {Id0 :: non_neg_integer(), Expr0 :: erl_parse:abstract_expr()}
  when
  Expr :: erl_parse:abstract_expr(),
  MfaSpec :: analyzer:mfa_spec(),
  FilterSpec :: filter(),
  Id :: non_neg_integer().
% Blocks.
trans_expr({block, Line, Body}, MfaSpec, FilterSpec, Id) ->
  {Id0, Body0} = trans_body(Body, MfaSpec, FilterSpec, Id),
  {Id0, {block, Line, Body0}};

% Pattern matching and assignments.
trans_expr({match, Line, Pattern, Expr}, MfaSpec, FilterSpec, Id) ->
  {Id0, Expr0} = trans_expr(Expr, MfaSpec, FilterSpec, Id),
  {Id0, {match, Line, Pattern, Expr0}};

% Unary operators.
trans_expr({op, Line, Op, Expr}, MfaSpec, FilterSpec, Id) ->
  {Id0, Expr0} = trans_expr(Expr, MfaSpec, FilterSpec, Id),
  {Id0, {op, Line, Op, Expr0}};

% Send and binary operators.
trans_expr(Expr = {op, _, '!', _, _}, MfaSpec, FilterSpec, Id) ->
  weave_send_expr(Expr, MfaSpec, FilterSpec, Id);
trans_expr({op, Line, Op, Expr1, Expr2}, MfaSpec, FilterSpec, Id) ->
  {Id0, Expr10} = trans_expr(Expr1, MfaSpec, FilterSpec, Id),
  {Id1, Expr20} = trans_expr(Expr2, MfaSpec, FilterSpec, Id0),
  {Id1, {op, Line, Op, Expr10, Expr20}};

% Tuples.
trans_expr({tuple, Line, Body}, MfaSpec, FilterSpec, Id) ->
  {Id0, Body0} = trans_body(Body, MfaSpec, FilterSpec, Id),
  {Id0, {tuple, Line, Body0}};

% Lists.
trans_expr({cons, Line, ExprHead, ExprTail}, MfaSpec, FilterSpec, Id) ->
  {Id0, ExprHead0} = trans_expr(ExprHead, MfaSpec, FilterSpec, Id),
  {Id1, ExprTail0} = trans_expr(ExprTail, MfaSpec, FilterSpec, Id0),
  {Id1, {cons, Line, ExprHead0, ExprTail0}};
trans_expr(Nil = {nil, _}, _, _, Id) ->
  {Id, Nil};

% Binaries.
trans_expr({bc, Line, Expr, Qualifiers}, MfaSpec, FilterSpec, Id) ->
  {Id0, Qualifiers0} = trans_qualifiers(Qualifiers, MfaSpec, FilterSpec, Id),
  {Id0, {bc, Line, Expr, Qualifiers0}};
trans_expr({bin, Line, BinElements}, MfaSpec, FilterSpec, Id) ->
  {Id0, BinElements0} = trans_bin_elements(BinElements, MfaSpec, FilterSpec, Id),
  {Id0, {bin, Line, BinElements0}};

% Maps.
trans_expr({map, Line, Associations}, MfaSpec, FilterSpec, Id) ->
  {Id0, Associations0} = trans_associations(Associations, MfaSpec, FilterSpec, Id),
  {Id0, {map, Line, Associations0}};
trans_expr({map, Line, Expr, Associations}, MfaSpec, FilterSpec, Id) ->
  {Id0, Expr0} = trans_expr(Expr, MfaSpec, FilterSpec, Id),
  {Id1, Associations0} = trans_associations(Associations, MfaSpec, FilterSpec, Id0),
  {Id1, {map, Line, Expr0, Associations0}};

% Records.
trans_expr({record, Line, Name, Fields}, MfaSpec, FilterSpec, Id) ->
  {Id0, Fields0} = trans_fields(Fields, MfaSpec, FilterSpec, Id),
  {Id0, {record, Line, Name, Fields0}};
trans_expr({record_field, Line, Expr, Name, Field}, MfaSpec, FilterSpec, Id) ->
  {Id0, Expr0} = trans_expr(Expr, MfaSpec, FilterSpec, Id),
  {Id0, {record_field, Line, Expr0, Name, Field}};
trans_expr({record_index, Line, Name, Field}, _, _, Id) ->
  {Id, {record_index, Line, Name, Field}};
trans_expr({record, Line, Expr, Name, Fields}, MfaSpec, FilterSpec, Id) ->
  {Id0, Expr0} = trans_expr(Expr, MfaSpec, FilterSpec, Id),
  {Id1, Fields0} = trans_fields(Fields, MfaSpec, FilterSpec, Id0),
  {Id1, {record, Line, Expr0, Name, Fields0}};

% Cases.
trans_expr({'case', Line, Expr, CaseClauses}, MfaSpec, FilterSpec, Id) ->
  {Id0, Expr0} = trans_expr(Expr, MfaSpec, FilterSpec, Id),
  {Id1, CaseClauses0} = trans_clauses(CaseClauses, MfaSpec, FilterSpec, Id0),
  {Id1, {'case', Line, Expr0, CaseClauses0}};

% Try and catch.
trans_expr({'try', Line, Body, CaseClauses, CatchClauses, BodyAfter}, MfaSpec, FilterSpec, Id) ->
  {Id0, Body0} = trans_body(Body, MfaSpec, FilterSpec, Id),
  {Id1, CaseClauses0} = trans_clauses(CaseClauses, MfaSpec, FilterSpec, Id0),
  {Id2, CatchClauses0} = trans_clauses(CatchClauses, MfaSpec, FilterSpec, Id1),
  {Id3, BodyAfter0} = trans_body(BodyAfter, MfaSpec, FilterSpec, Id2),
  {Id3, {'try', Line, Body0, CaseClauses0, CatchClauses0, BodyAfter0}};
trans_expr({'catch', Line, Expr}, MfaSpec, FilterSpec, Id) ->
  {Id0, Expr0} = trans_expr(Expr, MfaSpec, FilterSpec, Id),
  {Id0, {'catch', Line, Expr0}};

% Named and unnamed anonymous functions.
trans_expr({named_fun, Line, Name, Clauses}, MfaSpec, FilterSpec, Id) ->
  {Id0, Clauses0} = trans_clauses(Clauses, MfaSpec, FilterSpec, Id),
  {Id0, {named_fun, Line, Name, Clauses0}};
trans_expr({'fun', Line, {clauses, Clauses}}, MfaSpec, FilterSpec, Id) ->
  {Id0, Clauses0} = trans_clauses(Clauses, MfaSpec, FilterSpec, Id),
  {Id0, {'fun', Line, {clauses, Clauses0}}};

% Remote (external) function calls with spawn or spawn_link and other calls.
trans_expr(Expr = {call, _, {remote, _, {atom, _, erlang}, {atom, _, How}}, _}, MfaSpec, FilterSpec, Id)
  when How =:= spawn; How =:= spawn_link ->
  weave_remote_spawn(Expr, MfaSpec, FilterSpec, Id);

%% @private Remote (external) function calls with spawn or spawn_link. Added to
%% be able to instrument spawn and spawn_link calls for the 'proc_lib' library
%% in OTP. This particular fast addition/hack was added in a rush to be able to
%% instrument Ranch/Cowboy for the experiments required in FASE 2021.
trans_expr(Expr = {call, _, {remote, _, {atom, _, proc_lib}, {atom, _, How}}, _}, MfaSpec, FilterSpec, Id)
  when How =:= spawn; How =:= spawn_link ->

  ?TRACE("Matched proc_lib:~w", [How]),
  weave_remote_proc_lib_spawn(Expr, MfaSpec, FilterSpec, Id);

trans_expr({call, Line, {remote, Line0, ExprMod, ExprFun}, Args}, MfaSpec, FilterSpec, Id) ->
  {Id0, ExprMod0} = trans_expr(ExprMod, MfaSpec, FilterSpec, Id),
  {Id1, ExprFun0} = trans_expr(ExprFun, MfaSpec, FilterSpec, Id0),
  {Id2, Body0} = trans_args(Args, MfaSpec, FilterSpec, Id1),
  {Id2, {call, Line, {remote, Line0, ExprMod0, ExprFun0}, Body0}};

% Local (internal) function calls with spawn or spawn_link and other calls.
trans_expr(Expr = {call, _, {atom, _, How}, Args}, MfaSpec, FilterSpec, Id)
  when length(Args) =:= 3, How =:= spawn; How =:= spawn_link ->
  weave_local_spawn(Expr, MfaSpec, FilterSpec, Id);
trans_expr({call, Line, ExprF, Args}, MfaSpec, FilterSpec, Id) ->
  {Id0, Body0} = trans_args(Args, MfaSpec, FilterSpec, Id),
  {Id0, {call, Line, ExprF, Body0}};


% If conditions.
trans_expr({'if', Line, Clauses}, MfaSpec, FilterSpec, Id) ->
  {Id0, Clauses0} = trans_clauses(Clauses, MfaSpec, FilterSpec, Id),
  {Id0, {'if', Line, Clauses0}};

% List comprehensions.
trans_expr({lc, Line, Expr, Qualifiers}, MfaSpec, FilterSpec, Id) ->
  {Id0, Expr0} = trans_expr(Expr, MfaSpec, FilterSpec, Id),
  {Id1, Qualifiers0} = trans_qualifiers(Qualifiers, MfaSpec, FilterSpec, Id0),
  {Id1, {lc, Line, Expr0, Qualifiers0}};

% Receives.
trans_expr(Expr = {'receive', _, _}, MfaSpec, FilterSpec, Id) ->
  weave_receive_expr(Expr, MfaSpec, FilterSpec, Id);
trans_expr(Expr = {'receive', _, _, _, _}, MfaSpec, FilterSpec, Id) ->
  weave_receive_expr(Expr, MfaSpec, FilterSpec, Id);

% All other expressions.
trans_expr(Expr, _, _, Id) ->
  {Id, Expr}.

%% @private Transforms case, catch and function clauses.
-spec trans_clauses(Clauses, MfaSpec, FilterSpec, Id) ->
  {Id0 :: non_neg_integer(), Clauses0 :: [erl_parse:abstract_clause()]}
  when
  Clauses :: [erl_parse:abstract_clause()],
  MfaSpec :: analyzer:mfa_spec(),
  FilterSpec :: filter(),
  Id :: non_neg_integer().
trans_clauses([], _, _, Id) ->
  {Id, []};
trans_clauses([Clause | Clauses], MfaSpec, FilterSpec, Id) ->
  {Id0, Clause0} = trans_clause(Clause, MfaSpec, FilterSpec, Id),
  {Id1, Clauses0} = trans_clauses(Clauses, MfaSpec, FilterSpec, Id0),
  {Id1, [Clause0 | Clauses0]}.

%% @private Transforms case, catch and function clauses.
-spec trans_clause(Clause, MfaSpec, FilterSpec, Id) ->
  {Id0 :: non_neg_integer(), Clause0 :: erl_parse:abstract_clause()}
  when
  Clause :: erl_parse:abstract_clause(),
  MfaSpec :: analyzer:mfa_spec(),
  FilterSpec :: filter(),
  Id :: non_neg_integer().
trans_clause({clause, Line, Patterns, Guards, Body}, MfaSpec, FilterSpec, Id) ->
  {Id0, Body0} = trans_body(Body, MfaSpec, FilterSpec, Id),
  {Id0, {clause, Line, Patterns, Guards, Body0}}.

%% @private Transforms list and binary comprehensions.
trans_qualifiers([], _, _, Id) ->
  {Id, []};
trans_qualifiers([Qualifier | Qualifiers], MfaSpec, FilterSpec, Id) ->
  {Id0, Qualifier0} = trans_qualifier(Qualifier, MfaSpec, FilterSpec, Id),
  {Id1, Qualifiers0} = trans_qualifiers(Qualifiers, MfaSpec, FilterSpec, Id0),
  {Id1, [Qualifier0 | Qualifiers0]}.

%% @private Transforms list and binary comprehensions.
trans_qualifier({generate, Line, Pattern, Expr}, MfaSpec, FilterSpec, Id) ->
  {Id0, Expr0} = trans_expr(Expr, MfaSpec, FilterSpec, Id),
  {Id0, {generate, Line, Pattern, Expr0}};
trans_qualifier({b_generate, Line, Pattern, Expr}, MfaSpec, FilterSpec, Id) ->
  {Id0, Expr0} = trans_expr(Expr, MfaSpec, FilterSpec, Id),
  {Id0, {b_generate, Line, Pattern, Expr0}};
trans_qualifier(Expr, MfaSpec, FilterSpec, Id) ->
  trans_expr(Expr, MfaSpec, FilterSpec, Id).

%% @private Transforms remote and local function call arguments.
trans_args([], _, _, Id) ->
  {Id, []};
trans_args([Arg | Args], MfaSpec, FilterSpec, Id) ->
  {Id0, Arg0} = trans_arg(Arg, MfaSpec, FilterSpec, Id),
  {Id1, Args0} = trans_args(Args, MfaSpec, FilterSpec, Id0),
  {Id1, [Arg0 | Args0]}.

%% @private Transforms remote and local function call arguments.
trans_arg(Expr, MfaSpec, FilterSpec, Id) ->
  trans_expr(Expr, MfaSpec, FilterSpec, Id).

%% @private Transforms binary elements.
trans_bin_elements([], _, _, Id) ->
  {Id, []};
trans_bin_elements([Element | Elements], MfaSpec, FilterSpec, Id) ->
  {Id0, Element0} = trans_bin_element(Element, MfaSpec, FilterSpec, Id),
  {Id1, Elements0} = trans_bin_elements(Elements, MfaSpec, FilterSpec, Id0),
  {Id1, [Element0 | Elements0]}.

%% @private Transforms binary elements.
trans_bin_element({bin_element, Line, Expr, Size, Tsl}, MfaSpec, FilterSpec, Id) ->
  {Id0, Expr0} = trans_expr(Expr, MfaSpec, FilterSpec, Id),
  {Id0, {bin_element, Line, Expr0, Size, Tsl}}.

%% @private Transforms map associations.
trans_associations([], _, _, Id) ->
  {Id, []};
trans_associations([Association | Associations], MfaSpec, FilterSpec, Id) ->
  {Id0, Association0} = trans_association(Association, MfaSpec, FilterSpec, Id),
  {Id1, Associations0} = trans_associations(Associations, MfaSpec, FilterSpec, Id0),
  {Id1, [Association0 | Associations0]}.

%% @private Transforms map associations.
trans_association({map_field_assoc, Line, Key, Value}, MfaSpec, FilterSpec, Id) ->
  {Id0, Key0} = trans_expr(Key, MfaSpec, FilterSpec, Id),
  {Id1, Value0} = trans_expr(Value, MfaSpec, FilterSpec, Id0),
  {Id1, {map_field_assoc, Line, Key0, Value0}};
trans_association({map_field_exact, Line, Key, Value}, MfaSpec, FilterSpec, Id) ->
  {Id0, Key0} = trans_expr(Key, MfaSpec, FilterSpec, Id),
  {Id1, Value0} = trans_expr(Value, MfaSpec, FilterSpec, Id0),
  {Id1, {map_field_exact, Line, Key0, Value0}}.

%% @private Transforms record and typed fields.
trans_fields([], _, _, Id) ->
  {Id, []};
trans_fields([Field | Fields], MfaSpec, FilterSpec, Id) ->
  {Id0, Field0} = trans_field(Field, MfaSpec, FilterSpec, Id),
  {Id1, Fields0} = trans_fields(Fields, MfaSpec, FilterSpec, Id0),
  {Id1, [Field0 | Fields0]}.

%% @private Transforms record and typed fields.
trans_field({record_field, Line, Atom}, _, _, Id) ->
  {Id, {record_field, Line, Atom}};
trans_field({record_field, Line, Atom, Expr}, MfaSpec, FilterSpec, Id) ->
  {Id0, Expr0} = trans_expr(Expr, MfaSpec, FilterSpec, Id),
  {Id0, {record_field, Line, Atom, Expr0}};
trans_field({typed_record_field, {record_field, Line, Atom}, Type}, _, _, Id) ->
  {Id, {typed_record_field, {record_field, Line, Atom}, Type}};
trans_field({typed_record_field, {record_field, Line, Atom, Expr}, Type}, MfaSpec, FilterSpec, Id) ->
  {Id0, Expr0} = trans_expr(Expr, MfaSpec, FilterSpec, Id),
  {Id0, {typed_record_field, {record_field, Line, Atom, Expr0}, Type}}.


%%% ----------------------------------------------------------------------------
%%% Instrumentation weaving helper functions.
%%% ----------------------------------------------------------------------------

%% @private Weaves receive expressions.
weave_receive_expr({'receive', Line, Clauses}, MfaSpec, FilterSpec, Id) ->
  {Id0, Clauses0} = weave_receive_clauses(Clauses, MfaSpec, FilterSpec, Id),
  {Id0, {'receive', Line, Clauses0}};
weave_receive_expr({'receive', Line, Clauses, Expr, BodyAfter}, MfaSpec, FilterSpec, Id) ->
  {Id0, Clauses0} = weave_receive_clauses(Clauses, MfaSpec, FilterSpec, Id),
  {Id1, Expr0} = trans_expr(Expr, MfaSpec, FilterSpec, Id0),
  {Id2, BodyAfter0} = trans_body(BodyAfter, MfaSpec, FilterSpec, Id1),
  {Id2, {'receive', Line, Clauses0, Expr0, BodyAfter0}}.

%% @private Weaves receive expression clauses.
weave_receive_clauses([], _, _, Id) ->
  {Id, []};
weave_receive_clauses([Clause | CaseClauses], MfaSpec, FilterSpec, Id) ->
  {Id0, Clause0} = weave_receive_clause(Clause, MfaSpec, FilterSpec, Id),
  {Id1, CaseClauses0} = weave_receive_clauses(CaseClauses, MfaSpec, FilterSpec, Id0),
  {Id1, [Clause0 | CaseClauses0]}.

%% @private Weaves a receive clause.
weave_receive_clause({clause, Line, [Pattern], Guards, Body}, MfaSpec, FilterSpec, Id) ->

  % Transform receive clause body.
  {Id0, Body0} = trans_body(Body, MfaSpec, FilterSpec, Id),

  % Create variable to store message payload.
  Var = create_var(Line, 'Msg', Id0),
  Match = abs_match(Line, Var, Pattern),

  % Create trace event tuple used as arguments to monitor dispatch call. Event
  % is dispatched to monitor.
  SubmitCall = create_dispatch(Line, abs_tuple(Line, [
    abs_atom(Line, recv), create_self(Line), Var
  ])),

  % Create call expression to filter trace event and corresponding case
  % expression to apply the filter.
  FilterCall = wrap_fun_call(Line, FilterSpec, [Var]),
  FilterCase = abs_case(Line, FilterCall, [
    abs_case_clause(Line, [abs_atom(Line, true)], [], [SubmitCall]),
    abs_case_clause(Line, [abs_atom(Line, false)], [], [abs_atom(Line, ok)])
  ]),

  % Create case clause that replaces the original case clause in source AST.
  Clause = abs_case_clause(Line, [Match], Guards, [FilterCase | Body0]),
  {Id0 + 1, Clause}.

%% @private Weaves send expressions.
weave_send_expr({op, Line, Op = '!', Expr1, Expr2}, MfaSpec, FilterSpec, Id) ->

  % Transform expressions in left and right hand size operands of send.
  {Id0, Expr10} = trans_expr(Expr1, MfaSpec, FilterSpec, Id),
  {Id1, Expr20} = trans_expr(Expr2, MfaSpec, FilterSpec, Id0),

  % Create variables to store PID of receiver process and message payload. PID
  % and message are included in the send trace event tuple.
  Var1 = create_var(Line, 'Pid', Id1),
  Var2 = create_var(Line, 'Msg', Id1),
  Match1 = abs_match(Line, Var1, Expr10),
  Match2 = abs_match(Line, Var2, Expr20),

  % Create send expression that replaces the original send expression in source
  % AST.
  SendExpr0 = {op, Line, Op, Match1, Match2},

  % Create trace event tuple used as arguments to monitor dispatch call. Event
  % is dispatched to monitor.
  SubmitCall = create_dispatch(Line, abs_tuple(Line, [
    abs_atom(Line, send), create_self(Line), Var1, Var2])),

  % Create call expression to filter trace event and corresponding case
  % expression to apply the filter.
  FilterCall = wrap_fun_call(Line, FilterSpec, [Var2]),
  FilterCase = abs_case(Line, FilterCall, [
    abs_case_clause(Line, [abs_atom(Line, true)], [], [SubmitCall]),
    abs_case_clause(Line, [abs_atom(Line, false)], [], [Var2])
  ]),

  % Wrap the new expression and call into a new block.
  Block = abs_block(Line, [SendExpr0, FilterCase]),
  {Id1 + 1, Block}.

%% @private See {@link weave_remote_spawn/3}.
weave_local_spawn({call, Line, Fun = {atom, _, _}, Args}, MfaSpec, FilterSpec, Id) ->
  SpawnCall = abs_remote_call(Line, abs_atom(Line, erlang), Fun, Args),
  weave_remote_spawn(SpawnCall, MfaSpec, FilterSpec, Id).

%% @private Local spawn weaving is only limited to {@link erlang:spawn/3} since
%% the function `MfaSpec' accepts only MFA tuples. Handling anonymous functions
%% is hard, not because of the function itself, but because specifying MFA that
%% the monitor should be weaved in cannot be done easily.
%% Also, the weaving does not handle the case when the values of mod and fun are
%% not specified as atoms, but are obtained dynamically at runtime, since this
%% goes beyond the scope of static code weaving. It can however be easily
%% handled via tracing.
weave_remote_spawn({call, Line, {remote, _, ModSpawn = {atom, _, erlang}, FunSpawn = {atom, _, How}}, SpawnArgs}, MfaSpec, FilterSpec, Id)
  when How =:= spawn; How =:= spawn_link ->

  % For the particular function invocation spawn (or spawn_link), three
  % arguments are expected: module name, function name and argument list. Since
  % the spawn arguments can themselves be values that could be possibly obtained
  % via send or receive expressions, their value must be stored in variables.
  % Subsequent use of these values can be then made elsewhere, such as when the
  % spawn trace event is dispatched to the monitor. Saving the values the spawn
  % arguments evaluates to ensures that any side effects are performed precisely
  % once.
  [Mod, Fun, Args] = SpawnArgs,

  % Create variables that are used to bind the individual arguments of the
  % original call made to spawn. Variables are used to store the evaluations
  % of said arguments.
  ModMatch = abs_match(Line, ModVar = create_var(Line, 'Mod', Id), Mod),
  FunMatch = abs_match(Line, FunVar = create_var(Line, 'Fun', Id), Fun),
  ArgsMatch = abs_match(Line, ArgsVar = create_var(Line, 'Args', Id), Args),

  % The new arguments to original spawn call is now replaced with evaluated
  % arguments to spawn call; the values are those bound with above variables.
  SpawnArgsVars = [ModVar, FunVar, ArgsVar],

  % Expressions in original spawn call may themselves include send or receive.
  % Transform expressions. Id is incremented since list of arguments is
  % transformed.
  {Id0, SpawnArgs0} = trans_args([ModMatch, FunMatch, ArgsMatch], MfaSpec,
    FilterSpec, Id + 1),

  % Create MFA tuple that is used to determine whether a new monitor is to be
  % weaved around the the call to spawn.
  MfaTuple = abs_tuple(Line, SpawnArgs0),

  % Create call expression to check whether a monitor is to be weaved.
  MfaSpecCall = wrap_fun_call(Line, MfaSpec, [MfaTuple]),

  % Create variable that is used to bind the monitor function matched in the
  % case expression.
  MonFunVar = create_var(Line, 'MonFun', Id),

  % When weaving in monitor code into the source AST, the original spawn needs
  % to be wrapped with and additional function that store the monitor function
  % into the process dictionary of the process that is about to be spawned: this
  % allows the tracing/monitoring functions used later to retrieve said function
  % so that this can be applied on trace events. Should this function not exist
  % the the process dictionary, monitoring is simply skipped.
  % Create call expression that saves the monitor function (obtained via case)
  % and call to apply that applies the MFA used in the original call to spawn.
  PutCall = abs_local_call(
    Line, abs_atom(Line, put), [abs_atom(Line, ?MONITOR), MonFunVar]),
  ApplyCall = abs_local_call(Line, abs_atom(Line, apply), SpawnArgsVars),

  % Create variables to store PID of parent and child processes. PID of parent
  % process is used to populate the information in the 'init' event for the
  % child process; PID of child is used to populate the information in the
  % 'fork' event for the parent process.
  PidVar = create_var(Line, 'Pid', Id),
  PidParentVar = create_var(Line, 'PidParent', Id),

  % Create 'init' event for child process and dispatch to monitor.
  PidParentMatch = abs_match(Line, PidParentVar, create_self(Line)),
  SubmitCall0 = create_dispatch(Line, abs_tuple(Line, [
    abs_atom(Line, init), create_self(Line), PidParentVar, abs_tuple(Line, SpawnArgsVars)
  ])),

  % Create anonymous function to wrap in the call that saves the monitor into
  % the process dictionary, the call to dispatch the 'init' event, and the call
  % that applies the original MFA to be forked.
  WrapFun = abs_fun(Line, [abs_fun_clause(Line, [], [], [PutCall, SubmitCall0, ApplyCall])]),
  SpawnCall = abs_local_call(Line, abs_atom(Line, How), [WrapFun]),

  % Create 'fork' event for parent process and dispatch to monitor.
  SubmitCall = create_dispatch(Line, abs_tuple(Line, [
    abs_atom(Line, fork), create_self(Line), PidVar, abs_tuple(Line, SpawnArgsVars)
  ])),

  % Create case used to determine whether the monitor needs to be created. If
  % monitor does not need to be created, the original call to spawn from the
  % source AST is embedded as is; otherwise MFA in said original call to spawn
  % is wrapped around the host anonymous fun that includes all the bootstrapping
  % monitor code together with the 'init' event.
  Case = abs_case(Line, MfaSpecCall, [
    abs_case_clause(Line, [abs_atom(Line, undefined)], [], [abs_remote_call(Line, ModSpawn, FunSpawn, SpawnArgsVars)]),
    abs_case_clause(Line, [abs_tuple(Line, [abs_atom(Line, ok), MonFunVar])], [], [PidParentMatch, SpawnCall])
  ]),

  PidMatch = abs_match(Line, PidVar, Case),
  Block = abs_block(Line, [PidMatch, SubmitCall, PidVar]),
  {Id0, Block}.

%% @private Added to be able to instrument spawn and spawn_link calls for the
%% 'proc_lib' library in OTP. This particular fast addition/hack was added in a
%% rush to be able to instrument Ranch/Cowboy for the experiments required in
%% FASE 2021.
weave_remote_proc_lib_spawn({call, Line, {remote, _, ModSpawn = {atom, _, proc_lib}, FunSpawn = {atom, _, How}}, SpawnArgs}, MfaSpec, FilterSpec, Id)
  when How =:= spawn; How =:= spawn_link ->

  % For the particular function invocation spawn (or spawn_link), three
  % arguments are expected: module name, function name and argument list. Since
  % the spawn arguments can themselves be values that could be possibly obtained
  % via send or receive expressions, their value must be stored in variables.
  % Subsequent use of these values can be then made elsewhere, such as when the
  % spawn trace event is dispatched to the monitor. Saving the values the spawn
  % arguments evaluates to ensures that any side effects are performed precisely
  % once.
  [Mod, Fun, Args] = SpawnArgs,

  % Create variables that are used to bind the individual arguments of the
  % original call made to spawn. Variables are used to store the evaluations
  % of said arguments.
  ModMatch = abs_match(Line, ModVar = create_var(Line, 'Mod', Id), Mod),
  FunMatch = abs_match(Line, FunVar = create_var(Line, 'Fun', Id), Fun),
  ArgsMatch = abs_match(Line, ArgsVar = create_var(Line, 'Args', Id), Args),

  % The new arguments to original spawn call is now replaced with evaluated
  % arguments to spawn call; the values are those bound with above variables.
  SpawnArgsVars = [ModVar, FunVar, ArgsVar],

  % Expressions in original spawn call may themselves include send or receive.
  % Transform expressions. Id is incremented since list of arguments is
  % transformed.
  {Id0, SpawnArgs0} = trans_args([ModMatch, FunMatch, ArgsMatch], MfaSpec,
    FilterSpec, Id + 1),

  % Create MFA tuple that is used to determine whether a new monitor is to be
  % weaved around the the call to spawn.
  MfaTuple = abs_tuple(Line, SpawnArgs0),

  % Create call expression to check whether a monitor is to be weaved.
  MfaSpecCall = wrap_fun_call(Line, MfaSpec, [MfaTuple]),

  % Create variable that is used to bind the monitor function matched in the
  % case expression.
  MonFunVar = create_var(Line, 'MonFun', Id),

  % When weaving in monitor code into the source AST, the original spawn needs
  % to be wrapped with and additional function that store the monitor function
  % into the process dictionary of the process that is about to be spawned: this
  % allows the tracing/monitoring functions used later to retrieve said function
  % so that this can be applied on trace events. Should this function not exist
  % the the process dictionary, monitoring is simply skipped.
  % Create call expression that saves the monitor function (obtained via case)
  % and call to apply that applies the MFA used in the original call to spawn.
  PutCall = abs_local_call(
    Line, abs_atom(Line, put), [abs_atom(Line, ?MONITOR), MonFunVar]),
  ApplyCall = abs_local_call(Line, abs_atom(Line, apply), SpawnArgsVars),

  % Create variables to store PID of parent and child processes. PID of parent
  % process is used to populate the information in the 'init' event for the
  % child process; PID of child is used to populate the information in the
  % 'fork' event for the parent process.
  PidVar = create_var(Line, 'Pid', Id),
  PidParentVar = create_var(Line, 'PidParent', Id),

  % Create 'init' event for child process and dispatch to monitor.
  PidParentMatch = abs_match(Line, PidParentVar, create_self(Line)),
  SubmitCall0 = create_dispatch(Line, abs_tuple(Line, [
    abs_atom(Line, init), create_self(Line), PidParentVar, abs_tuple(Line, SpawnArgsVars)
  ])),

  % Create anonymous function to wrap in the call that saves the monitor into
  % the process dictionary, the call to dispatch the 'init' event, and the call
  % that applies the original MFA to be forked.
  WrapFun = abs_fun(Line, [abs_fun_clause(Line, [], [], [PutCall, SubmitCall0, ApplyCall])]),
  SpawnCall = abs_remote_call(Line, abs_atom(Line, proc_lib), abs_atom(Line, How), [WrapFun]),
%%  PidMatch = abs_match(Line, PidVar, SpawnCall),

%%  IoFormatCall = abs_remote_call(Line, abs_atom(Line, io), abs_atom(Line, format), [abs_string(Line, "Monitor instrumented for MFA ~w in process ~w.~n"), abs_list(Line, [abs_tuple(Line, SpawnArgsVars), PidVar])]),
%%  IoFormatCall2 = abs_remote_call(Line, abs_atom(Line, io), abs_atom(Line, format), [abs_string(Line, "Monitor NOT instrumented for MFA ~w.~n"), abs_list(Line, [abs_tuple(Line, SpawnArgsVars)])]),

  %%  Call = abs_remote_call(Line, , [abs_string(Line, "Hello ~p ~p"), abs_list(Line, [abs_atom(Line, test), abs_list(Line, [abs_string(Line, "Duncan"), abs_integer(Line, 36)])])]),

  % Create 'fork' event for parent process and dispatch to monitor.
  SubmitCall = create_dispatch(Line, abs_tuple(Line, [
    abs_atom(Line, fork), create_self(Line), PidVar, abs_tuple(Line, SpawnArgsVars)
  ])),

  % Create case used to determine whether the monitor needs to be created. If
  % monitor does not need to be created, the original call to spawn from the
  % source AST is embedded as is; otherwise MFA in said original call to spawn
  % is wrapped around the host anonymous fun that includes all the bootstrapping
  % monitor code together with the 'init' event.
  Case = abs_case(Line, MfaSpecCall, [
    abs_case_clause(Line, [abs_atom(Line, undefined)], [], [abs_remote_call(Line, ModSpawn, FunSpawn, SpawnArgsVars)]),
%%    abs_case_clause(Line, [abs_tuple(Line, [abs_atom(Line, ok), MonFunVar])], [], [PidParentMatch, PidMatch, SubmitCall])
    abs_case_clause(Line, [abs_tuple(Line, [abs_atom(Line, ok), MonFunVar])], [], [PidParentMatch, SpawnCall])
  ]),

  PidMatch = abs_match(Line, PidVar, Case),
  Block = abs_block(Line, [PidMatch, SubmitCall, PidVar]),
  {Id0, Block}.


%%% ----------------------------------------------------------------------------
%%% Abstract syntax generating helper functions.
%%% TODO: Replace with functions from the erl_parse module.
%%% ----------------------------------------------------------------------------

abs_atom(Line, Atom) ->
  {atom, Line, Atom}.

abs_char(Line, Char) ->
  {char, Line, Char}.

abs_float(Line, Float) ->
  {float, Line, Float}.

abs_integer(Line, Integer) ->
  {integer, Line, Integer}.

abs_string(Line, String) ->
  {string, Line, String}.

abs_remote_call(Line, Mod, Fun, Args) ->
  {call, Line, {remote, Line, Mod, Fun}, Args}.

abs_local_call(Line, Fun, Args) ->
  {call, Line, Fun, Args}.

abs_fun(Line, Clauses) ->
  {'fun', Line, abs_fun_clauses(Clauses)}.

abs_fun_clauses(Clauses) ->
  {clauses, Clauses}.

abs_fun_clause(Line, Patterns, Guards, Body) ->
  {clause, Line, Patterns, Guards, Body}.

abs_list(Line, []) ->
  {nil, Line};
abs_list(_, [Expr | Exprs]) ->
  Line = element(2, Expr), % Second element of expression is ALWAYS line number.
  {cons, Line, Expr, abs_list(Line, Exprs)}.

abs_tuple(Line, Exprs) ->
  {tuple, Line, Exprs}.

abs_match(Line, Pattern, Expr) ->
  {match, Line, Pattern, Expr}.

abs_var(Line, Var) ->
  {var, Line, Var}.

abs_block(Line, Body) ->
  {block, Line, Body}.

abs_case(Line, Expr, CaseClauses) ->
  {'case', Line, Expr, CaseClauses}.

abs_case_clause(Line, Patterns, Guards, Body) ->
  {clause, Line, Patterns, Guards, Body}.


%%% ----------------------------------------------------------------------------
%%% Builder helper functions.
%%% ----------------------------------------------------------------------------

%% @private Returns the abstract syntax for a call to {@link erlang:self/0}.
create_self(Line) ->
  abs_local_call(Line, abs_atom(Line, self), []).

%% @private Returns the abstract syntax for a variable with the specified ID.
create_var(Line, Name, Id) ->
  abs_var(Line,
    list_to_atom(lists:flatten(io_lib:format("_~s@~b", [Name, Id])))).

%% @private Returns the abstract syntax for a dispatching an event to the
%% monitor.
create_dispatch(Line, Event) ->
  abs_remote_call(Line, abs_atom(Line, ?TRACE_MOD), abs_atom(Line, ?TRACE_FUN),
    [Event]).

wrap_fun_call(Line, Fun, Args) ->
  case erlang:fun_info(Fun, type) of
    {type, local} ->

      % Fun is specified in anonymous form. Unwrap the abstract syntax of
      % fun body to create full function call. Said function is embedded as is
      % in the target code.
      {env, [{[], {eval, _}, {value, _}, Body}]} = erlang:fun_info(Fun, env),
      Fun0 = abs_fun(Line, Body),

      % Create local call that applies the function to arguments.
      abs_local_call(Line, Fun0, Args);
    {type, external} ->

      % Fun specified in normal m:f/a form. Create call that applies function
      % to arguments.
      {Mod0, Fun0, 1} = erlang:fun_info_mfa(Fun),
      abs_remote_call(Line, abs_atom(Line, Mod0), abs_atom(Line, Fun0), Args)
  end.

%%erl_syntax:atom_value({atom, 10, hello}).
%%erl_eval:expr({cons,36,{integer,36,20},{nil,36}}, erl_eval:new_bindings()).


%% Limitations.
% 1. If the message does not match a receive clause, this is not traced and
% forwarded to the monitor for analysis.
% 2. The message is traced and forwarded to the monitor for analysis only when
% it is dequeued from the mailbox.
% 3. It is very very difficult to handle exit messages, since the monitor code
% is integrated into the process itself, and when the process dies normally or
% is killed externally, the monitor dies immediately with the process. This
% cannot be solved by changing the process into a system process, as it might
% have adverse effects on the rest of the system architecture when supervisors
% and process linking is involved, because the instrumented process no longer
% fails when it should.
% 4. Inlined monitors cannot be garbage collected unless the host process dies,
% not even when a verdict is reached (they must loop);
% outline monitors can be garbage collected when a verdict is reached. This
% might warrant a change in the dynamics of the instrumentation relation that
% may either loop or exit.

%%Result = weaver:weave("src/models", "include", "ebin2", fun({M, F, [X]}) when X > 9, X < 200 -> {ok, fun(Event) -> 'yes' end}; (_) -> undefined end).
%%Result = weaver:weave("src/models", "include", "ebin2", fun({slave, init, [Id, NumMsgs]}) -> {ok, fun Mon(Event) -> Mon end}; (_) -> undefined end).


