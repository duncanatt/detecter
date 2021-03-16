%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Example client exposing API to the calculator service.
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
-module(calc_client).
-author("Duncan Paul Attard").

%%% Public API.
-export([add/2, mul/2]).
-export([rpc/2]).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Issues addition request to server.
%%
%% {@params
%%   {@name A}
%%   {@desc First number to add.}
%%   {@name B}
%%   {@desc Second number to add.}
%% }
%%
%% {@returns Result of addition.}
-spec add(A :: number(), B :: number()) -> number().
add(A, B) ->
  {add, Res} = rpc(calc_server, {add, A, B}),
  Res.

%% @doc Issues multiplication request to server.
%%
%% {@params
%%   {@name A}
%%   {@desc First number to multiply.}
%%   {@name B}
%%   {@desc Second number to multiply.}
%% }
%%
%% {@returns Result of multiplication.}
-spec mul(A :: number(), B :: number()) -> number().
mul(A, B) ->
  {mul, Res} = rpc(calc_server, {mul, A, B}),
  Res.

%% @doc Issues a synchronous request that blocks until a response is received.
%%
%% {@params
%%   {@name To}
%%   {@desc PID or registered name of target process.}
%%   {@name Req}
%%   {@desc Request to issue.}
%% }
%%
%% {@returns Response.}
-spec rpc(To :: pid() | atom(), Req :: any()) -> any().
rpc(To, Req) ->
  To ! {self(), Req},
  receive
    Resp ->
      Resp
  end.