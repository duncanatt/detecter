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
-export([start_online/3, start_offline/3, stop/0]).
-export([start_off/4]).


%%% Callbacks/Internal.
-export([]).

%%% Types.
-export_type([]).

%%% Implemented behaviors.
%-behavior().


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(OPT_PARENT, parent).
-define(OPT_ANALYSIS, analysis).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type option() :: {?OPT_PARENT, tracer:parent()} | {?OPT_ANALYSIS, tracer:a_mode()}.

-type options() :: list(option()).

%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% {@returns Root monitor PID.}
%%-spec start_online(Mfa, MfaSpec, Opts) -> pid()
%%  when
%%  Mfa :: mfa(),
%%  MfaSpec :: analyzer:mfa_spec(),
%%  Opts :: proplists:proplist().
start_online({Mod, Fun, Args}, MfaSpec, Opts) when is_function(MfaSpec, 1) ->

  % Start tracing framework.
  trace_lib:start(evm),

  % Launch system in a blocked state, and wait for ack from launching process.
  % Syn is sent prior the system starts executing in order to prevent the
  % potential loss of trace events.
  Self = self(),
  Pid = spawn(fun() -> util:syn(Self), apply(Mod, Fun, Args) end),

  % Start root tracer for system. System is currently blocked: this prevents
  % trace events from being lost.
  Merged = proplists:get_value(merged, Opts, false),
  Root =
    if Merged =:= false -> tracer:start(Pid, MfaSpec, external, self);
      true -> tracer:start(Pid, MfaSpec, internal, self)
    end,

  % Ack root monitor and system, now that the former has been fully started.
  util:syn_ack(Root),
  util:syn_ack(Pid),
  Root.

%% {@returns Root monitor PID.}
%%-spec start_offline(File, PidS, MfaSpec) -> pid()
%%  when
%%  File :: string(),
%%  PidS :: pid(),
%%  MfaSpec :: analyzer:mfa_spec().
start_offline(File, PidS, MfaSpec) when is_function(MfaSpec, 1) ->

  % Set up controller monitor. The controller collects all the tracer statistics
  % and monitor verdicts at their time of exit by relying process linking.
%%  Self = self(),

  % Wait for controller to start up and ack before continuing.
%%  util:syn(Controller),

  % Start tracing framework.
  trace_lib:start({log, File}),

  % Start root tracer for system. Since this is offline monitoring, the trace
  % is assumed to exist. As in the online case, the root tracer is bootstrapped
  % by specifying the root process of the system (obtained from the trace log).
%%  Merged = proplists:get_value(merged, Opts, false),
  Root = tracer:start(PidS, MfaSpec, external, self),

  % Ack root monitor.
  util:syn_ack(Root),
  Root.


start_off(File, PidS, MfaSpec, Opts) when is_function(MfaSpec, 1) ->

  Parent = parent_opt(Opts),
  Analysis = analysis_opt(Opts),

  % Start tracing framework.
  trace_lib:start({log, File}),

  Root = tracer:start(PidS, MfaSpec, Analysis, Parent),

  % Ack root monitor.
  util:syn_ack(Root),
  Root.

-spec stop() -> ok | {error, not_started}.
stop() ->
  %% TODO: Update to modern syntax using try-catch block.
  case catch unregister(?MODULE) of
    true ->
      ok;
    {'EXIT', _} ->
      io:format("Unable to unregister ~s; already terminated.~n", [?MODULE])
  end,
  tracer:stop(),
  trace_lib:stop().

%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------


parent_opt(Opts) ->
  proplists:get_value(?OPT_PARENT, Opts, self).

analysis_opt(Opts) ->
  proplists:get_value(?OPT_ANALYSIS, Opts, internal).