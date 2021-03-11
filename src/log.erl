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
-module(log).
-author("Duncan Paul Attard").

%%% Includes.
-include("log.hrl").

%%% Public API.
-export([log_to_file/1, write/4, write/5]).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Configures the logger to output to the specified file.
%%
%% {@params
%%   {@name File}
%%   {@desc Full filename where the logger output is directed.}
%% }
%%
%% {@returns `true' indicating that log file is configured.}
-spec log_to_file(File :: file:filename()) -> true.
log_to_file(File) ->
  {ok, Log} = file:open(File, [write]),
  erlang:group_leader(Log, self()).

%% @doc Outputs the log statement.
%%
%% {@params
%%   {@name LogLabel}
%%   {@desc Label indicating the severity of the log statement.}
%%   {@name Module}
%%   {@desc Name of the module issuing the log statement.}
%%   {@name Line}
%%   {@desc Line number of the log statement.}
%%   {@name Fmt}
%%   {@desc Format of the log statement string.}
%% }
%%
%% {@returns `ok' to acknowledge success.}
-spec write(LogLabel, Module, Line, Fmt) -> ok
  when
  LogLabel :: string(),
  Module :: atom(),
  Line :: integer(),
  Fmt :: string().
write(LogLabel, Module, Line, Fmt) ->
  write(LogLabel, Module, Line, Fmt, []).

%% @doc Outputs the log statement with formatting parameters.
%%
%% {@params
%%   {@name LogLabel}
%%   {@desc Label indicating the severity of the log statement.}
%%   {@name Module}
%%   {@desc Name of the module issuing the log statement.}
%%   {@name Line}
%%   {@desc Line number of the log statement.}
%%   {@name Fmt}
%%   {@desc Format of the log statement string.}
%%   {@name Params}
%%   {@desc Formatting parameters for the log statement string.}
%% }
%%
%% {@returns `ok' to acknowledge success.}
-spec write(LogLabel, Module, Line, Fmt, Params) -> ok
  when
  LogLabel :: string(),
  Module :: atom(),
  Line :: integer(),
  Fmt :: string(),
  Params :: list().
write(LogLabel, Module, Line, Fmt, Params) ->
  case can_log(?log_level, LogLabel) of
    true ->
      io:fwrite(user, "[~s - ~p - ~p:~p] - ~s~n",
        [LogLabel, self(), Module, Line, io_lib:format(Fmt, Params)]);
    false -> ok
  end.


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% @private Determines whether a log statement for the specified severity level
%% can be output.
%%
%% {@params
%%   {@name LogLevel}
%%   {@desc Level from 1 to 5, indicating the severity of the log statement,
%%          where 1 = TRACE, 2 = DEBUG, 3 = INFO, 4 = WARN, 5 = ERROR.
%%   }
%%   {@name LogLabel}
%%   {@desc Label indicating the severity of the log statement.}
%% }
%%
%% {@returns `true' if the log statement can be output, otherwise `false'.}
-spec can_log(LogLevel :: integer(), LogLabel :: string()) -> boolean().
can_log(?trace_level, ?trace_str) ->
  true;
can_log(?trace_level, ?debug_str) ->
  true;
can_log(?trace_level, ?info_str) ->
  true;
can_log(?trace_level, ?warn_str) ->
  true;
can_log(?trace_level, ?error_str) ->
  true;

can_log(?debug_level, ?debug_str) ->
  true;
can_log(?debug_level, ?info_str) ->
  true;
can_log(?debug_level, ?warn_str) ->
  true;
can_log(?debug_level, ?error_str) ->
  true;

can_log(?info_level, ?info_str) ->
  true;
can_log(?info_level, ?warn_str) ->
  true;
can_log(?info_level, ?error_str) ->
  true;

can_log(?warn_level, ?warn_str) ->
  true;
can_log(?warn_level, ?error_str) ->
  true;

can_log(?error_level, ?error_str) ->
  true;
can_log(_, _) -> false.