%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Common front-end library for different tracing infrastructures.
%%%
%%% {@par The following tracing methods are supported:
%%%   {@dl
%%%     {@item `evm'}
%%%     {@desc Native Erlang Virtual Machine tracing.}
%%%     {@item `@{log, File@}'}
%%%     {@desc Log file tracing that emulates the functionality exposed by the
%%%            Erlang Virtual Machine for seamless swapping. Trace event
%%%            descriptions are obtained from the specified log `File'. See
%%%            {@link log_tracer} for more information.
%%%     }
%%%   }
%%% }
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
-module(trace_lib).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([start/1, stop/0]).
-export([trace/1, clear/1, preempt/1]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Available tracing infrastructure.
-define(REG_TRACERS, #{
  evm => evm_tracer, % Erlang Virtual Machine tracer.
  log => log_tracer % Log file tracer.
}).

%% Trace library persistent term key used to detect whether the tracer has been
%% started or stopped.
-define(MOD_KEY, '$trace_lib_mod').


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type trace_method() :: evm | {log, File :: file:filename()}.
%% Supported tracing methods:
%% <dl>
%%   <dt>`evm'</dt>
%%   <dd>Native Erlang Virtual Machine tracing.</dd>
%%   <dt>`{log, File}'</dt>
%%   <dd>
%%     Log file tracing that emulates the functionality exposed by the
%%     Erlang Virtual Machine for seamless swapping. Trace event
%%     descriptions are obtained from the specified log `File'. See
%%     {@link log_tracer} for more information.
%%   </dd>
%% </dl>


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Configures and starts the tracer.
%%
%% {@params
%%   {@name Method}
%%   {@desc The tracing infrastructure to use.}
%% }
%%
%% {@par The following infrastructures are supported:
%%   {@dl
%%     {@term `evm'}
%%     {@desc Native Erlang Virtual Machine tracing.}
%%     {@term `@{log, File@}'}
%%     {@desc Log file tracing that emulates the functionality exposed by the
%%            Erlang Virtual Machine. `File' should point to the log file where
%%            the trace event descriptions reside. See {@link log_tracer} for
%%            more information.
%%     }
%%   }
%% }
%% {@returns Depends on the tracing `Method' used:
%%   {@dl
%%     {@term `ok'}
%%     {@desc When `Method' is set to `evm'. }
%%     {@term `@{ok, Pid@}'}
%%     {@desc When `Method' is set to `log'. `Pid' is the PID of the tracing
%%            service process.
%%     }
%%   }
%% }
-spec start(Method :: trace_method()) -> ok | {ok, pid()}.
start(evm) ->
  try_start(evm_tracer, start, []);
start({log, File}) ->
  try_start(log_tracer, start, [File]).

%% @doc Shuts down the tracer.
%%
%% {@returns `ok' to indicate successful shut down, `@{error, not_started@}'
%%            otherwise.
%% }
-spec stop() -> ok | {error, not_started}.
stop() ->
  case persistent_term:get(?MOD_KEY, undefined) of
    undefined ->
      {error, not_started};
    Mod ->
      persistent_term:erase(?MOD_KEY),
      Mod:stop()
  end.

%% @doc Sets the caller process as the tracer for the specified tracee PID.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc PID of the process to trace.}
%% }
%% {@par Call fails if another process is already tracing `Tracee'. The call is
%%       non-blocking.
%% }
%%
%% {@returns `true' if successful, otherwise `false'.}
-spec trace(Tracee :: pid()) -> boolean().
trace(Tracee) when is_pid(Tracee) ->
  (get_mod()):trace(Tracee).

%% @doc Clears the tracer for the specified tracee PID.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc PID of the process whose tracer is to be cleared.}
%% }
%%
%% {@returns `true' regardless of whether a tracer was set up for `Tracee'.}
-spec clear(Tracee :: pid()) -> boolean().
clear(Tracee) when is_pid(Tracee) ->
  (get_mod()):clear(Tracee).

%% @doc Establishes the calling process as the new tracer of the specified
%% tracee PID.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc PID of the process whose tracer is to be preempted.}
%% }
%% {@par Call fails if no tracer is set up for `Tracee'.}
%%
%% {@returns `true' if successful, otherwise `false'.}
-spec preempt(Tracee :: pid()) -> boolean().
preempt(Tracee) when is_pid(Tracee) ->
  (get_mod()):preempt(Tracee).


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% @private Saves module name as persistent term.
-spec put_mod(Mod :: module()) -> ok.
put_mod(Mod) ->
  persistent_term:put(?MOD_KEY, Mod).

%% @private Retrieves module name from persistent storage.
-spec get_mod() -> module() | no_return().
get_mod() ->
  persistent_term:get(?MOD_KEY).

%% @private Determines whether the tracing infrastructure is started.
-spec try_start(Mod :: module(), Fun :: atom(), Args :: list()) ->
  ok | {ok, Pid :: pid()} | {error, {already_started, undefined | pid()}}.
try_start(Mod, Fun, Args) ->
  case persistent_term:get(?MOD_KEY, undefined) of
    undefined ->
      put_mod(Mod),
      apply(Mod, Fun, Args);
    Mod0 ->
      {error, {already_started, whereis(Mod0)}}
  end.