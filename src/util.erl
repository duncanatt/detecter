%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Provides a number of utility functions.
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
-module(util).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([rpc/2, rpc_async/2, rpc_yield/1, rpc_yield/2]).

%%% Callbacks/Internal.
-export([]).

%%% Types.
-export_type([]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Issues a synchronous request that blocks until a corresponding response
%%      is received.
%%
%% {@params
%%   {@name To}
%%   {@desc PID or registered name of target process.}
%%   {@name Req}
%%   {@desc Request to issue.}
%% }
%%
%% {@returns Response.}
-spec rpc(To :: pid() | atom(), Req :: term()) -> term().
rpc(To, Req) when is_pid(To); is_atom(To) ->
  Ref = erlang:make_ref(),
  To ! {self(), Ref, Req},
  receive
    {Ref, Resp} ->
      Resp
  end.

%% @doc Issues an asynchronous request.
%%
%% {@params
%%   {@name To}
%%   {@desc PID or registered name of target process.}
%%   {@name Req}
%%   {@desc Request to issue.}
%% }
%%
%% {@par The presence of a corresponding response can be checked using either
%%       {@link rpc_yield/1} or {@link rpc_yield/2}.
%% }
%%
%% {@returns Unique reference identifying the issued request.}
-spec rpc_async(To :: pid() | atom(), Req :: term()) -> reference().
rpc_async(To, Req) when is_pid(To); is_atom(To) ->
  Ref = erlang:make_ref(),
  To ! {self(), Ref, Req},
  Ref.

%% @doc Blocks until the response identified by the specified reference is
%%      received.
%%
%% {@params
%%   {@name Ref}
%%   {@desc Response reference.}
%% }
%%
%% {@returns Corresponding response.}
-spec rpc_yield(Ref :: reference()) -> term().
rpc_yield(Ref) when is_reference(Ref) ->
  rpc_yield(Ref, infinity).

%% @doc Receives the response identified by the specified reference.
%%
%% {@params
%%   {@name Ref}
%%   {@desc Response reference.}
%%   {@name TimeoutMs}
%%   {@desc Amount of milliseconds to wait before timing out.}
%% }
%%
%% {@returns Corresponding response, otherwise `@{timeout, Ref@}' if not received
%%           within the stipulated timeout window.
%% }
-spec rpc_yield(Ref, TimeoutMs) -> term() | {timeout, reference()}
  when
  Ref :: reference(),
  TimeoutMs :: non_neg_integer() | infinity.
rpc_yield(Ref, TimeoutMs) when
  is_reference(Ref), is_integer(TimeoutMs), TimeoutMs >= 0;
  TimeoutMs =:= infinity ->
  receive
    {Ref, Resp} ->
      Resp
  after TimeoutMs ->
    {timeout, Ref}
  end.


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------
