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
-module(simple).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([start/0, stop/0]).

%%% Callbacks/Internal.
-export([init/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

start() ->
  register(?MODULE, Pid = spawn(?MODULE, init, [])),
  Pid.

stop() ->
  ?MODULE ! {self(), stop}.

%%% ----------------------------------------------------------------------------
%%% Internal callbacks.
%%% ----------------------------------------------------------------------------

init() ->
  process_flag(trap_exit, true),
  loop(0).

loop(N) ->
  receive
    {_From, stop} ->
      io:format("[~w] Stopping server.~n", [N]);
    {From, Req} ->
      io:format("[~w] Received request ~p from ~w.~n", [N, Req, From]),
      From ! {self(), ok_received},
      loop(N + 1);
    Any ->
      io:format("[~w] Received unknown message ~p.~n", [N, Any]),
      loop(N + 1)
  end.
