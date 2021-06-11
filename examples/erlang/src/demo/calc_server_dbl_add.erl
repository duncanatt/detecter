%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Example buggy calculator service that expects two addition requests.
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
-module(calc_server_dbl_add).
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
    {Clt1, {add, A1, B1}} ->

      % Handle first addition request from client.
      Clt1 ! {ok, A1 + B1},

      % Wait for second addition request.
      receive
        {Clt2, {add, A2, B2}} ->

          % Handle first addition request from client.
          Clt2 ! {ok, A2 - B2}, % Bug!!
          loop(Tot + 2)
      end;

    {Clt, {mul, A, B}} ->

      % Handle multiplication request from client.
      Clt ! {ok, A * B},
      loop(Tot + 1);

    {Clt, stp} ->

      % Handle stop request. Server does not loop again.
      Clt ! {bye, Tot}
  end.