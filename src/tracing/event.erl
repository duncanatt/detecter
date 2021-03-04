%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Intermediate to Erlang Virtual Machine trace event translation.
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
-module(event).
-author("Duncan Paul Attard").

%%% Public API.
-export([to_evm_event/1]).

%%% Types.
-export_type([int_event/0, evm_event/0]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type int_event() ::
{fork, Parent :: pid(), Child :: pid(), Mfa :: mfa()} |
{init, Child :: pid(), Parent :: pid(), Mfa :: mfa()} |
{exit, Process :: pid(), Reason :: term()} |
{send, Sender :: pid(), Receiver :: pid(), Message :: term()} |
{recv, Receiver :: pid(), Message :: term()}.
%% Intermediate trace event format agnostic of the tracer implementation. See
%% {@link evm_event/0} for trace events specific to the Erlang Virtual Machine.

-type evm_event() ::
{trace, PidSrc :: pid(), spawn, PidTgt :: pid(), Mfa :: mfa()} |
{trace, PidSrc :: pid(), spawned, PidTgt :: pid(), Mfa :: mfa()} |
{trace, PidSrc :: pid(), exit, Reason :: term()} |
{trace, PidSrc :: pid(), send, Msg :: term(), PidTgt :: pid()} |
{trace, PidSrc :: pid(), 'receive', Msg :: term()}.
%% Trace event format issued by the Erlang Virtual Machine tracing
%% infrastructure. See {@link erlang:trace/3} for more information.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Translates the trace event in intermediate representation to Erlang
%% Virtual Machine tracer equivalent.
%%
%% {@params
%%   {@name Event}
%%   {@desc Intermediate trace event representation to translate.}
%% }
%%
%% {@returns Translated event.}
-spec to_evm_event(Event :: int_event()) -> evm_event().
to_evm_event({fork, Parent, Child, Mfa}) ->
  {trace, Parent, spawn, Child, Mfa};
to_evm_event({init, Child, Parent, Mfa}) ->
  {trace, Child, spawned, Parent, Mfa};
to_evm_event({exit, Process, Reason}) ->
  {trace, Process, exit, Reason};
to_evm_event({send, Sender, Receiver, Msg}) ->
  {trace, Sender, send, Msg, Receiver};
to_evm_event({recv, Receiver, Msg}) ->
  {trace, Receiver, 'receive', Msg}.
