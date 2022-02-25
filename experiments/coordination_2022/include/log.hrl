%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Logging macros acting as the fronted for the the functions in log.erl.
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
%%% Client configuration macros.
%%% ----------------------------------------------------------------------------

%% Comment the line below to disable logging.
-define(log, log).

%% -----------------------------------------------------------------------------
%% The macro log_level controls the logging level. It can be set to a value
%% from 1 to 4 (inclusive). Log statements are output if the value of
%% log_level is greater or equal to the logging level of the log statement being
%% output.
%% There are 4 logging levels:
%% 1 = TRACE
%% 2 = DEBUG
%% 3 = INFO
%% 4 = WARN
%% 5 = ERROR
%%
%% Example:
%%   If the log statement being output is DEBUG (i.e. level 2), and the current
%%   logging level is that of TRACE (i.e. log_level = 1), then the log is output
%%    to the screen because the level for DEBUG is greater than that of TRACE.
%% Example:
%%   If the log statement being output is INFO (i.e. level 3), but the current
%%   logging level is that of ERROR (i.e. log_level = 4), then the log is not
%%   output to the screen because the level for INFO is less than that of
%%   ERROR. To be able to see the log, the log level must be set to any one of
%%   TRACE (i.e. log_level = 1), DEBUG (i.e. log_level = 2) or INFO
%%   (i.e. log_level = 3).
%% -----------------------------------------------------------------------------
-ifndef(log_level).
-define(log_level, 3).
-endif.


%%% ----------------------------------------------------------------------------
%%% Client API macros.
%%% ----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Provides a simple and clean logging interface that can be used to output
%% logging statements on screen.
%%
%% The available macros
%% Example usage:
%%   ?INFO("Hello my name is ~p ~p.", [duncan, attard]) where:
%%   the format string includes two parameters, shown by ~p, whose values are
%%   given in the list [duncan, attard] (works like the standard io:format/1-3
%%   functions).
%% The short form when no parameters are required is:
%%   ?INFO("Hello world!").
%% -----------------------------------------------------------------------------
-ifdef(log).

-define(on_true(Cond, Term), case Cond of true -> Term; _ -> ok end).

%% Logging macro expanded to log output when the LOG macro is defined.
-define(TRACE(Fmt, Params), log:write(?trace_str, ?MODULE, ?LINE, Fmt, Params)).
-define(DEBUG(Fmt, Params), log:write(?debug_str, ?MODULE, ?LINE, Fmt, Params)).
-define(INFO(Fmt, Params), log:write(?info_str, ?MODULE, ?LINE, Fmt, Params)).
-define(WARN(Fmt, Params), log:write(?warn_str, ?MODULE, ?LINE, Fmt, Params)).
-define(ERROR(Fmt, Params), log:write(?error_str, ?MODULE, ?LINE, Fmt, Params)).
-define(TRACE(Msg), ?TRACE(Msg, [])).
-define(DEBUG(Msg), ?DEBUG(Msg, [])).
-define(INFO(Msg), ?INFO(Msg, [])).
-define(WARN(Msg), ?WARN(Msg, [])).
-define(ERROR(Msg), ?ERROR(Msg, [])).
-define(TRACE(Fmt, Params, Cond), ?on_true(Cond, ?TRACE(Fmt, Params))).
-define(DEBUG(Fmt, Params, Cond), ?on_true(Cond, ?DEBUG(Fmt, Params))).
-define(INFO(Fmt, Params, Cond), ?on_true(Cond, ?INFO(Fmt, Params))).
-define(WARN(Fmt, Params, Cond), ?on_true(Cond, ?WARN(Fmt, Params))).
-define(ERROR(Fmt, Params, Cond), ?on_true(Cond, ?ERROR(Fmt, Params))).
-else.
-define(TRACE(Fmt, Params, Cond), ok).
-define(DEBUG(Fmt, Params, Cond), ok).
-define(INFO(Fmt, Params, Cond), ok).
-define(WARN(Fmt, Params, Cond), ok).
-define(ERROR(Fmt, Params, Cond), ok).
-define(TRACE(Fmt, Params), ok).
-define(DEBUG(Fmt, Params), ok).
-define(INFO(Fmt, Params), ok).
-define(WARN(Fmt, Params), ok).
-define(ERROR(Fmt, Params), ok).
-define(TRACE(Fmt), ok).
-define(DEBUG(Fmt), ok).
-define(INFO(Fmt), ok).
-define(WARN(Fmt), ok).
-define(ERROR(Fmt), ok).

-endif.


%%% ----------------------------------------------------------------------------
%%% Private macros (DO NOT MODIFY).
%%% ----------------------------------------------------------------------------

%% Logging level definitions.
-define(trace_level, 1).
-define(debug_level, 2).
-define(info_level, 3).
-define(warn_level, 4).
-define(error_level, 5).

%% Logging level string definitions.
-define(trace_str, "TRACE").
-define(debug_str, "DEBUG").
-define(info_str, "INFO").
-define(warn_str, "WARN").
-define(error_str, "ERROR").

