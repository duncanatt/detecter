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
-module(token_server).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").

%%% Public API.
-export([start/1]).

%%% Callbacks/Internal.
-export([loop/2]).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Starts server.
%%
%% {@params
%%   {@name Tok}
%%   {@desc Token ID to start the server with.}
%% }
%%
%% {@returns Server PID.}
-spec start(Tok :: integer()) -> pid().
start(Tok) ->
  spawn(?MODULE, loop, [Tok, Tok]).


%%% ----------------------------------------------------------------------------
%%% Internal callbacks.
%%% ----------------------------------------------------------------------------

%% @private Main server loop.
%%
%% {@params
%%   {@name OwnTok}
%%   {@desc Private server token.}
%%   {@name NextTok}
%%   {@desc Next token to be issued to client.}
%% }
%%
%% {@returns Does not return.}
-spec loop(OwnTok :: integer(), NextTok :: integer()) -> no_return().
loop(OwnTok, NextTok) ->
  receive
    {Clt, 0} ->
      Clt ! NextTok,
      io:format("(~w) Serving token request with ~w.~n", [OwnTok, NextTok]),
      loop(OwnTok, NextTok + 1)
  end.