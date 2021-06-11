%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Example buggy calculator service.
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
-module(calc_server_bug).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").

%%% Public API.
-export([start/1]).

%%% Internal callbacks.
-export([loop/1]).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Starts server.
%%
%% {@params
%%   {@name N}
%%   {@desc The request number to start the server with.}
%% }
%%
%% {@returns Server PID.}
-spec start(N :: integer()) -> pid().
start(N) ->
  spawn(?MODULE, loop, [N]).


%%% ----------------------------------------------------------------------------
%%% Internal callbacks.
%%% ----------------------------------------------------------------------------

%% @private Main server loop.
%%
%% {@params
%%   {@name Tot}
%%   {@desc Total number of serviced requests.}
%% }
%%
%% {@returns Does not return.}
-spec loop(Tot :: integer()) -> no_return().
loop(Tot) ->
  receive
    {Clt, {add, A, B}} ->

      % Handle addition request from client.
      Clt ! {ok, A - B}, % Bug!!
      loop(Tot + 1);

    {Clt, {mul, A, B}} ->

      % Handle multiplication request from client.
      Clt ! {ok, A * B},
      loop(Tot + 1);

    {Clt, stp} ->

      % Handle stop request. Server does not loop again.
      Clt ! {bye, Tot}
  end.