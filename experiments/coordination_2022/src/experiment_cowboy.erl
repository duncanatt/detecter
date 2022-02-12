%%%-------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%% @copyright (C) 2020 Duncan Paul Attard
%%% @version 0.9
%%%
%%% @doc
%%%
%%% @end
%%% Created: 25. Sep 2020
%%% 
%%% Copyright (c) 2020 Duncan Paul Attard <duncanatt@gmail.com>
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------
-module(experiment_cowboy).
-author("Duncan Paul Attard").
-version("0.9").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").

%%% Public API exports.
-export([start/0, stop/0]).

%%% Internal exports.
-export([]).

%%% Type exports.
-export_type([]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Type declarations.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

start() ->
  DepsStat = [application:start(Dep) || Dep <- [crypto, asn1, public_key, ssl, ranch, cowlib, cowboy]],
  io:format("Deps loaded: ~p.~n", [DepsStat]),

%%  hello_erlang_app:start([], []).
%%  file_server_app:start([], []).
  token_app:start([], []).

stop() ->
  ok.


%%% ----------------------------------------------------------------------------
%%% Internal exports.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Callbacks.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% evm_tracer:start().
%% Tracer = fun(Pid) -> io:format("Starting tracing system ~p.~n", [Pid]), erlang:trace(all, true, [send, 'receive', procs]), timer:sleep(1000), Pid ! ok, io:format("Sent unblocking message to ~p.~n", [Pid]), Loop = fun Loop() -> receive Msg -> io:format("MSG: ~w~n", [Msg]) end, Loop() end, Loop() end.
%% Tracer = fun(Pid) -> io:format("Starting tracing system ~p.~n", [Pid]), erlang:trace(Pid, true, [set_on_spawn, set_on_link, send, 'receive', procs]), timer:sleep(1000), Pid ! ok, io:format("Sent unblocking message to ~p.~n", [Pid]), Loop = fun Loop() -> receive Msg -> io:format("MSG: ~w~n", [Msg]) end, Loop() end, Loop() end.
%% Tracer = fun(Pid) -> io:format("Starting tracing system ~p.~n", [Pid]), erlang:trace(new_processes, true, [set_on_spawn, set_on_link, send, 'receive', procs, call]), timer:sleep(1000), Pid ! ok, io:format("Sent unblocking message to ~p.~n", [Pid]), Loop = fun Loop() -> receive Msg -> io:format("MSG: ~w~n", [Msg]) end, Loop() end, Loop() end.
%% Pid = spawn(fun() -> receive ok -> io:format("Unblocked!~n") end, experiment_cowboy:start(), timer:sleep(20000) end).
%% spawn(fun() -> Tracer(Pid) end).

%% To compile the monitor and instrument the system.
%% hml_eval:compile("examples/cowboy_mon.hml", [{outdir, "ebin"}, v]).
%% weaver:weave("src/cowboy", fun cowboy_mon:mfa_spec/1, [{outdir, "ebin"}, {i, "include"}]).
%% weaver:weave("src/ranch", fun cowboy_mon:mfa_spec/1, [{outdir, "ebin"}, {i, "include"}]).


