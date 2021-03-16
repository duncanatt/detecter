%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Active file polling and parsing of trace event logs.
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
-module(log_poller).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([start/2, start_link/2, stop/0]).

%%% Callbacks.
-export([init/1, handle_line/3]).

%%% Implemented behaviors.
-behavior(gen_file_poller).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type state() :: {events_read, Cnt :: non_neg_integer()}.
%% Total events parsed.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Starts the standalone log file poller.
%%
%% {@params
%%   {@name File}
%%   {@desc Source file to monitor for changes.}
%%   {@name Opts}
%%   {@desc File poller configuration options. Only one option is supported:
%%          {@dl
%%            {@term `@{poll_ms, Ms@}'}
%%            {@desc Polling interval in milliseconds. Defaults to 500 ms.}
%%          }
%%   }
%% }
%%
%% {@returns `@{ok, Pid@}', where `Pid' is the PID of the file poller process.}
-spec start(File, Opts) -> no_return()
  when
  File :: file:filename(),
  Opts :: gen_file_poller:options().
start(File, Opts) ->
  gen_file_poller:start({local, ?MODULE}, ?MODULE, File, Opts).

%% @doc Starts the log file poller as part of a supervision tree.
%%
%% {@params
%%   {@name File}
%%   {@desc Source file to monitor for changes.}
%%   {@name Opts}
%%   {@desc File poller configuration options. Only one option is supported:
%%          {@dl
%%            {@term `@{poll_ms, Ms@}'}
%%            {@desc Polling interval in milliseconds. Defaults to 500 ms.}
%%          }
%%   }
%% }
%%
%% {@returns `@{ok, Pid@}', where `Pid' is the PID of the file poller process.}
-spec start_link(File, Opts) -> no_return()
  when
  File :: file:filename(),
  Opts :: gen_file_poller:options().
start_link(File, Opts) ->
  gen_file_poller:start_link({local, ?MODULE}, ?MODULE, File, Opts).

%% @doc Shuts down the log file poller.
%%
%% {@returns Does not return in case of errors, otherwise `ok' to indicate a
%%           successful shut down.
%% }
-spec stop() -> ok | no_return().
stop() ->
  gen_file_poller:stop(?MODULE).


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% @private Handles the log poller initialization.
%%
%% {@params
%%   {@name Args}
%%   {@desc List of arguments, currently unused.}
%% }
%%
%% {@returns Log file poller start state.}
-spec init(Args :: list()) -> {ok, State :: state()}.
init(_Args) ->
  {ok, {events_read, 0}}.

%% @private Parses one trace event description from its list of bytes, and
%% posts the event to the log tracer.
%%
%% {@params
%%   {@name Line}
%%   {@desc Trace event description as a list of bytes.}
%%   {@name LineInfo}
%%   {@desc Line position information in source log file.}
%%   {@name State}
%%   {@desc Log poller state.}
%% }
%%
%% {@returns Log file poller state.}
-spec handle_line(Line, LineInfo, State) -> {ok, NewState :: state()}
  when
  Line :: [byte()],
  LineInfo :: gen_file_poller:line_info(),
  State :: state().
handle_line(Line, LineInfo, State = {events_read, Cnt}) ->
  LineNum = gen_file_poller:line_info_line_num(LineInfo),
  case log_eval:eval_string(Line, LineNum) of
    {ok, skip} ->

      % Event skipped because the evaluated string was a chunk of whitespace or
      % was commented out.
      {ok, State};
    {ok, Event} ->

      % A valid event has been evaluated successfully. Post it to log tracer.
      log_tracer:post_event(Event),
      ?TRACE("~4..0B Event ~w.", [LineNum, Event]),
      {ok, {events_read, Cnt + 1}}
  end.
