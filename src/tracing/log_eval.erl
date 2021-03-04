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
-module(log_eval).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([eval_string/2]).

%%% Callbacks.
-export([]).

%%% Types.
-export_type([]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type line_num() :: pos_integer().
%% Line number of parsed token in source log file.

-type ev_atom() :: {atom, line_num(), atom()}.
%% Atom token.

-type ev_int() :: {int, line_num(), integer()}.
%% Integer token.

-type ev_float() :: {float, line_num(), float()}.
%% Float token.

-type ev_pid() :: {pid, line_num(), pid()}.
%% PID token.

-type ev_string() :: {string, line_num(), string()}.
%% String token.

-type ev_list() :: nil | {cons, ev_term(), nil} | {cons, ev_term(), ev_list()}.
%% List AST node.

-type ev_tuple() :: {tuple, [ev_term()]}.
%% Tuple AST node.

-type ev_term() :: ev_atom() | ev_int() | ev_float() | ev_pid() |
ev_string() | ev_list() | ev_tuple().
%% Term AST node.

-type ev_mfa() :: {mfa, ev_atom(), ev_atom(), ev_list()}.
%% MFA node.

-type ev_fork() :: {fork, ev_pid(), ev_pid(), ev_mfa()}.
%% Fork trace event AST node.

-type ev_init() :: {init, ev_pid(), ev_pid(), ev_mfa()}.
%% Init trace event AST node.

-type ev_exit() :: {exit, ev_pid(), ev_atom()}.
%% Exit trace event AST node.

-type ev_send() :: {send, ev_pid(), ev_pid(), ev_term()}.
%% Send trace event AST node.

-type ev_recv() :: {recv, ev_pid(), ev_term()}.
%% Receive trace event AST node.

-type ev_event() :: ev_fork() | ev_init() | ev_exit() | ev_send() | ev_recv().
%% Trace event AST node.

-type ev_delay() :: {delay, Ms :: ev_int(), Event :: ev_event()}.

%% TODO: What to do with this? In which module shall I put it?
-type log() :: {delay, Ms :: timeout(), Event :: event:event()}.

%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Converts the specified trace event specification string to its native
%% Erlang form.
%%
%% {@params
%%   {@name String}
%%   {@desc The stringified representation of the trace event specification. See
%%          {@section Trace events} for the expected trace event specification
%%          format.
%%   }
%% }
%%
%% {@returns the parsed trace event specification in Erlang form.}
-spec eval_string(String, LineNum) -> {ok, skip} | {ok, log()} | no_return()
  when
  String :: string(),
  LineNum :: line_num().
eval_string(String, LineNum) when is_list(String) ->
  case log_lexer:string(String) of
    {ok, [], _} ->
      {ok, skip};
    {ok, Tokens, _} ->
      case log_parser:parse(Tokens) of
        {ok, Ast} ->
          {ok, eval_delay(Ast)};
        {error, {_, _, ErrorDesc}} ->
          error({error, LineNum, log_parser:format_error(ErrorDesc)})
      end;
    {error, {_, _, ErrorDesc}, _} ->
      error({error, LineNum, log_lexer:format_error(ErrorDesc)})
  end.


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

-spec eval_delay(Delay :: ev_delay() | ev_event()) -> log().
eval_delay({delay, {int, _, Ms}, Event}) ->
  {delay, Ms, eval_event(Event)};
eval_delay(Event) ->
  {delay, 0, eval_event(Event)}.

-spec eval_event(Event :: ev_event()) -> event:event().
eval_event({fork, {pid, _, Pid}, {pid, _, Pid2}, Mfa}) ->
  {fork, Pid, Pid2, eval_mfa(Mfa)};
eval_event({init, {pid, _, Pid}, {pid, _, Pid2}, Mfa}) ->
  {init, Pid, Pid2, eval_mfa(Mfa)};
eval_event({exit, {pid, _, Pid}, {atom, _, Reason}}) ->
  {exit, Pid, Reason};
eval_event({send, {pid, _, Pid}, {pid, _, Pid2}, Item}) ->
  {send, Pid, Pid2, eval_term(Item)};
eval_event({recv, {pid, _, Pid}, Item}) ->
  {recv, Pid, eval_term(Item)}.

-spec eval_mfa(Mfa :: ev_mfa()) -> mfa().
eval_mfa({mfa, {atom, _, M}, {atom, _, F}, Args}) ->
  {M, F, eval_term(Args)}.

-spec eval_term(Term :: ev_term()) ->
  pid() | atom() | integer() | float() | list() | tuple().
eval_term({pid, _, Pid}) ->
  Pid;
eval_term({atom, _, Atom}) ->
  Atom;
eval_term({int, _, Int}) ->
  Int;
eval_term({float, _, Float}) ->
  Float;
eval_term({string, _, String}) ->
  String;
eval_term(nil) ->
  [];
eval_term({cons, Head, Tail}) ->
  [eval_term(Head) | eval_term(Tail)];
eval_term({tuple, []}) ->
  {};
eval_term({tuple, Terms}) ->
  list_to_tuple(conv_list(Terms)).

-spec conv_list(Terms :: list(ev_delay())) -> list().
conv_list([]) ->
  [];
conv_list([Term | Terms]) ->
  [eval_term(Term) | conv_list(Terms)].
