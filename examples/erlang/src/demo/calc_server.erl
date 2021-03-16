%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Example calculator service.
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
-module(calc_server).
-author("Duncan Paul Attard").

%%% Public API.
-export([start/1, stop/0]).

%%% Internal callbacks.
-export([loop/1]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Normal and buggy operation constants.
-define(MODE_OK, ok).
-define(MODE_BUGGY, buggy).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type mode() :: ?MODE_OK | ?MODE_BUGGY.
%% Server operation mode.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Starts server.
%%
%% {@params
%%   {@name Mode}
%%   {@desc Server operation mode}
%% }
%%
%% {@par The server operates in one of two modes: `ok', where it handles
%%       calculations correctly, and `buggy', to return incorrect results.
%% }
%%
%% {@returns Server PID.}
-spec start(Mode :: mode()) -> pid().
start(Mode) ->
  register(?MODULE, Pid = spawn(?MODULE, loop, [
    if Mode =:= ?MODE_OK -> 0; Mode =:= ?MODE_BUGGY -> 1 end
  ])),
  Pid.

%% @doc Stops server.
%%
%% {@returns `stopped' to indicate successful termination.}
-spec stop() -> ok.
stop() ->
  {ok, Status} = calc_client:rpc(?MODULE, stop),
  Status.


%%% ----------------------------------------------------------------------------
%%% Internal callbacks.
%%% ----------------------------------------------------------------------------

%% @private Main server loop.
%%
%% {@params
%%   {@name ErrFact}
%%   {@desc Error factor used to displace and return the correct or incorrect
%%          result of the calculation.
%%   }
%% }
%%
%% {@returns Does not return.}
-spec loop(ErrFact :: number()) -> no_return().
loop(ErrFact) ->
  receive
    {From, {add, A, B}} ->

      % Handle addition request from client.
      From ! {add, A + B + ErrFact},
      loop(ErrFact);

    {From, {mul, A, B}} ->

      % Handle multiplication request from client.
      From ! {mul, A * B + ErrFact},
      loop(ErrFact);

    {From, stop} ->

      % Handle stop request. Server does not loop again.
      From ! {ok, stopped};

    Any ->
      io:format("WARN: unknown request ~w.~n", [Any]),
      loop(ErrFact)
  end.