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
-module(monitor).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([start_online/3, start_offline/4, stop/0]).
%%-export([start_off/4]).

%%% Types.
-export_type([option/0, options/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Option definitions.
-define(OPT_PARENT, parent).
-define(OPT_ANALYSIS, analysis).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type option() :: {?OPT_PARENT, tracer:parent()} | {?OPT_ANALYSIS, tracer:a_mode()}.
%% Monitor options.

-type options() :: list(option()).
%% Monitor option list.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Starts and instruments the specified system with online monitors.
%%
%% {@params
%%   {@name @{Mod, Fun, Args@}}
%%   {@desc }
%%   {@name MfaSpec}
%%   {@desc Function that determines whether an analyzer is associated with a
%%          MFA whose process instantiation needs to be monitored.
%%   }
%%   {@name Opts}
%%   {@desc To be filled later.}
%% }
%%
%% {@returns PID of root monitor.}
-spec start_online({Mod, Fun, Args}, MfaSpec, Opts) -> pid()
  when
  Mod :: module(),
  Fun :: atom(),
  Args :: list(),
  MfaSpec :: analyzer:mfa_spec(),
  Opts :: options().
start_online({Mod, Fun, Args}, MfaSpec, Opts) when is_function(MfaSpec, 1) ->

  % Start tracing framework.
  trace_lib:start(evm),

  % Syn is issued before so that the system is executed one the root monitor
  % has ack'ed. This prevents potential trace event loss.
  Self = self(),
  PidS = spawn(
    fun() ->
      ?TRACE("Started bootstrapping process.."),

      util:syn(Self),
      Return = apply(Mod, Fun, Args),
      util:rpc_async(Self, {return, Return}),

      ?TRACE("Bootstrapping process terminated.")
    end),

  % Start root tracer with specified options.
  Root = tracer:start(PidS, MfaSpec, analysis_opt(Opts), parent_opt(Opts)),

  % Ack root monitor and system, now that the former has been fully started.
  util:syn_ack(Root),
  util:syn_ack(PidS),

  % Obtain return result of applied function.
  {ok, Root, receive {_, _, {return, Return}} -> Return end}.

%% @doc Loads the offline trace from the specified file and replay it with offline
%% monitors.
%%
%% {@params
%%   {@name File}
%%   {@desc Full path of log file containing the trace event descriptions.}
%%   {@name PidS}
%%   {@desc PID of the top-level system process.}
%%   {@name MfaSpec}
%%   {@desc Function that determines whether an analyzer is associated with a
%%          MFA whose process instantiation needs to be monitored.
%%   }
%%   {@name Opts}
%%   {@desc To be filled later.}
%% }
%%
%% {@returns PID of root monitor.}
-spec start_offline(File, PidS, MfaSpec, Opts) -> pid()
  when
  File :: file:filename(),
  PidS :: pid(),
  MfaSpec :: analyzer:mfa_spec(),
  Opts :: option().
start_offline(File, PidS, MfaSpec, Opts) when is_function(MfaSpec, 1) ->

  % Start tracing framework.
  trace_lib:start({log, File}),

  % Start root tracer with specified options. In offline monitoring, the trace
  % is already assumed to exist. As is done for the online case, the root tracer
  % is bootstrapped with the PID of the top-level system process.
  Root = tracer:start(PidS, MfaSpec, analysis_opt(Opts), parent_opt(Opts)),

  % Ack root monitor.
  util:syn_ack(Root),
  Root.


%%start_off(File, PidS, MfaSpec, Opts) when is_function(MfaSpec, 1) ->
%%
%%  Parent = parent_opt(Opts),
%%  Analysis = analysis_opt(Opts),
%%
%%  % Start tracing framework.
%%  trace_lib:start({log, File}),
%%
%%  Root = tracer:start(PidS, MfaSpec, Analysis, Parent),
%%
%%  % Ack root monitor.
%%  util:syn_ack(Root),
%%  Root.

online_attach(Pid, MfaSpec, Opts) when is_function(MfaSpec, 1) ->
  % TODO: (Future). Attach to process once the system has been started.
  ok.

online_detach(Pid) ->
  % TODO: (Future). Stop analysing process.
  ok.


%% @doc Shuts down the monitors.
%%
%% {@returns `ok' to indicate successful shut down, `@{error, not_started@}'
%%            otherwise.
%% }
-spec stop() -> ok | {error, not_started}.
stop() ->
  tracer:stop(),
  trace_lib:stop().

%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% @doc Returns the user-specified parent option.
%%
%% {@params
%%   {@name Opts}
%%   {@desc Monitor options list.}
%% }
%%
%% {@returns User-specified option or `self' if undefined.}
-spec parent_opt(Opts :: options()) -> {?OPT_PARENT, tracer:parent()}.
parent_opt(Opts) ->
  proplists:get_value(?OPT_PARENT, Opts, self).

%% @doc Returns the user-specified analysis mode option.
%%
%% {@params
%%   {@name Opts}
%%   {@desc Monitor options list.}
%% }
%%
%% {@returns User-specified option or `internal' if undefined.}
-spec analysis_opt(Opts :: options()) -> {?OPT_ANALYSIS, tracer:a_mode()}.
analysis_opt(Opts) ->
  proplists:get_value(?OPT_ANALYSIS, Opts, internal).