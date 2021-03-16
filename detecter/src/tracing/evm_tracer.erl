%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Wrapper for the Erlang Virtual Machine tracing infrastructure.
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
-module(evm_tracer).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([start/0, stop/0, trace/1, clear/1, preempt/1]).

-ifdef(TEST).
-export([get_tracer/1]).
-endif.


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Default trace flags for process communication and lifecycle events.
-define(TRC_FLAGS, [send, 'receive', procs]).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Configures the tracer.
-spec start() -> ok.
start() ->
  1 = config_tracer(),
  ok.

%% @doc Shuts down the tracer for all existing processes.
-spec stop() -> ok.
stop() ->
  erlang:trace(all, false, [set_on_spawn | ?TRC_FLAGS]),
  ok.

%% @doc Sets the caller process as the tracer for tracee.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc PID of the process to trace.}
%% }
%%
%% {@returns `true' if successful, or `false' if `Tracee' is already being
%%            traced.}
-spec trace(Tracee :: pid()) -> boolean().
trace(Tracee) when is_pid(Tracee) ->
  trace(Tracee, true).

%% @doc Clears the tracer for the specified tracee.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc PID of the process whose tracer is to be cleared.}
%% }
%%
%% {@returns `true' to acknowledge that `Tracee' is no longer being traced.}
-spec clear(pid()) -> true.
clear(Tracee) when is_pid(Tracee) ->
  trace(Tracee, false),
  true.

%% @doc Sets the caller process as the new tracer for tracee.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc PID of the process to trace.}
%% }
%%
%% {@returns `true' if successful, or `false' if `Tracee' is not being traced.}
preempt(Tracee) when is_pid(Tracee) ->
  try

    % Suspend tracer to minimise trace event loss due to process actions.
    erlang:suspend_process(Tracee),

    try

      % Tracer successfully suspended. Calling process stops the old tracer from
      % tracing Tracee, starts tracing Tracee itself, and then resumes Tracee.
      trace(Tracee, false) and trace(Tracee, true) and
        erlang:resume_process(Tracee)
    catch
      error:badarg ->

        % Tracee not resumed: it does not exist, has already exited, or has a
        % suspend count < 0.
        ?WARN("Tracee ~w not resumed.", [Tracee]),
        false
    end
  catch
    error:badarg ->

      % Tracee not suspended: it does not exist or has already exited.
      ?WARN("Tracee ~w not suspended.", [Tracee]),
      false
  end.


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% @private Turns on (if `How == true') or off (if `How == false') tracing for
%% the specified tracee PID.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc The PID of the process to trace.}
%% }
%% {@par The call fails if another process is already tracing `Tracee'.}
%%
%% {@returns `true' if the operation succeeded, otherwise `false'.}
-spec trace(Tracee :: pid(), How :: boolean()) -> boolean().
trace(Tracee, How) ->
  try

    % Calling process becomes the default tracer. Tracee is traced using the
    % default flags; the tracer is additionally set to be automatically
    % inherited by all processes spawned by Tracee unless otherwise changed by
    % preempt/1.
    1 =:= erlang:trace(Tracee, How, [set_on_spawn | ?TRC_FLAGS])
  catch
    error:badarg ->
      ?WARN("Unable to set trace flags to '~p' for non-existent process ~p.",
        [How, Tracee]),
      false
  end.

%% @private Configures the Erlang Virtual Machine trace pattern flags for `send'
%% and `receive' events.
%%
%% {@returns `1' to indicate the configuration of the trace pattern flags.}
-spec config_tracer() -> 1.
config_tracer() ->

  % Configure send and receive trace patterns to filter out unwanted trace event
  % messages such as code loading and outputting out to the console.
  erlang:trace_pattern('receive', [
    {
      ['_', '_', '$1'], % Match
      [ % Guard: only matches tuples that fulfill criteria.
        {is_tuple, '$1'},
        {'=/=', {element, 1, '$1'}, 'ctrl'}, % and
        {'=/=', {element, 1, '$1'}, 'io_reply'}, % and
        {'=/=', {element, 1, '$1'}, 'io_request'}, % and
        {'=/=', {element, 1, '$1'}, 'code_server'}, % and
        {'=/=', {element, 1, '$1'}, '$gen_call'}, % and
        {'=/=', {element, 1, '$1'}, '$syn'}, % and
        {'=/=', {element, 1, '$1'}, '$ack'}, % and
        {'=/=', {element, 1, '$1'}, '$syn_ack'}, % and
        {'=/=', {element, 1, '$1'}, '$log'}, % and
        {'=/=', {element, 1, '$1'}, '$cmd'}%, % and
%%        {'not', {'is_reference', {element, 1, '$1'}}} % and, not of is_reference of the first element of the tuple.
      ],
      [] % Action
    },
    { % OR
      ['_', '_', '$1'], % Match
      [ % Guard: only matches atoms that fulfill criteria.
        {is_atom, '$1'},
        {'=/=', 'timeout', '$1'}
      ],
      [] % Action
    },
    { % OR
      ['_', '_', '$1'], % Match
      [ % Guard: only matches with one of these data types.
        {'orelse', % All is_* guards are in disjunction.
          {is_number, '$1'},
          {is_list, '$1'},
          {is_pid, '$1'},
          {is_port, '$1'},
          {is_reference, '$1'},
          {is_map, '$1'},
          {is_binary, '$1'},
          {is_function, '$1'}
        }
      ],
      [] % Action
    }
  ],
    []),

  erlang:trace_pattern(send, [
    {
      ['_', '$1'], % Match
      [ % Guard: only matches tuples that fulfill criteria.
        {is_tuple, '$1'},
        {'=/=', {element, 1, '$1'}, 'ctrl'}, % and
        {'=/=', {element, 1, '$1'}, 'io_request'}, % and
        {'=/=', {element, 1, '$1'}, 'io_reply'}, % and
        {'=/=', {element, 1, '$1'}, 'code_call'}, % and
        {'=/=', {element, 1, '$1'}, '$gen_call'}, % and
        {'=/=', {element, 1, '$1'}, '$syn'}, % and
        {'=/=', {element, 1, '$1'}, '$ack'}, % and
        {'=/=', {element, 1, '$1'}, '$syn_ack'}, % and
        {'=/=', {element, 1, '$1'}, '$log'}, % and
        {'=/=', {element, 1, '$1'}, '$cmd'}%, % and
%%        {'not', {'is_reference', {element, 1, '$1'}}} % and, not of is_reference of the first element of the tuple.
      ],
      [] % Action
    },
    { % OR
      ['_', '$1'], % Match
      [ % Guard: only matches with one of these data types.
        {'orelse', % All is_* guards are in disjunction.
          {is_atom, '$1'},
          {is_number, '$1'},
          {is_list, '$1'},
          {is_pid, '$1'},
          {is_port, '$1'},
          {is_reference, '$1'},
          {is_map, '$1'},
          {is_binary, '$1'},
          {is_function, '$1'}
        }
      ],
      [] % Action
    }
  ],
    []).


%%% ----------------------------------------------------------------------------
%%% Testing.
%%% ----------------------------------------------------------------------------

-ifdef(TEST).

%% @doc Determines whether tracee is being traced.
%%
%% {@params
%%   {@name Tracee}
%%   {@desc PID of the process to check.}
%% }
%%
%% {@returns PID of the associated tracer or `undefined' if `Tracee' is not
%%           being traced.
%% }
-spec get_tracer(Tracee :: pid()) -> pid() | undefined.
get_tracer(Tracee) ->
  case erlang:trace_info(Tracee, tracer) of
    {tracer, []} ->
      undefined; % Tracee defined but tracer undefined.
    {tracer, Tracer} ->
      Tracer;
    undefined ->
      undefined % Tracee undefined.
  end.

-endif.

%% Just receive.
%%Pid = spawn(fun() -> Loop = fun Loop(N) -> receive spawn -> _Pid = spawn(fun() -> ok, receive _ -> ok end end), io:format("Spawned: ~p.~n", [_Pid]); Msg -> io:format("[~p] Received ~p.~n", [N, Msg]), Loop(N + 1) end end, Loop(0) end).

%% Receive with reply.
%%Pid = spawn(fun() -> Loop = fun Loop(N) -> receive spawn -> _Pid = spawn(fun() -> ok, receive _ -> ok end end), io:format("Spawned: ~p.~n", [_Pid]); {From, Msg} -> io:format("[~p] Received ~p from ~p.~n", [N, Msg, From]), From ! {self(), ok_reply}, Loop(N + 1) end end, Loop(0) end).

%% Tracer.
%%TrcPid = spawn(fun() -> trace_lib:start(evm), trace_lib:trace(Pid), Loop = fun Loop(N) -> receive Msg -> io:format("[~p] TRC Received event ~p.~n", [N, Msg]), Loop(N + 1) end end, Loop(0) end).