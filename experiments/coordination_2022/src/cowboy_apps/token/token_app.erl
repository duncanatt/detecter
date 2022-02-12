%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Module description (becomes module heading).
%%%
%%% @end
%%% 
%%% Copyright (c) 2022, Duncan Paul Attard <duncanatt@gmail.com>
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
-module(token_app).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([start/2, stop/1]).

%%% Callbacks/Internal.
-export([]).

%%% Types.
-export_type([]).

%%% Implemented behaviors.
%-behavior().


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

start(_Type, _Args) ->

  Dispatch = cowboy_router:compile([
    {'_', Routes = [{"/token", token_handler, []}]}
  ]),

  % Set the Ranch transport options for the TCP layer.
  TransOpts = #{num_acceptors => 10, socket_opts => [{port, 8080}]},

  % Set the Ranch protocol options for the HTTP layer.
  ProtoOpts = #{env => #{dispatch => Dispatch}},

  % Start Cowboy over plain HTTP.
  {ok, _} = cowboy:start_clear(my_http_listener, TransOpts, ProtoOpts),
  io:format("Running Cowboy with Ranch options: ~w.~n", [TransOpts]),
  io:format("Configured routes: ~w.~n", [Routes]),

  token_sup:start_link().


stop(_State) ->
  ok = cowboy:stop_listener(http).