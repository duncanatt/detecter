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
-export([syn/1, syn_ack/1]).
-export([promise/1, then/1]).
-export([as_dir_name/1]).

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
%% {@returns Reference identifying the issued request.}
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

%% @doc Issues a synchronization request that initiates the first phase of a
%% three-way handshake.
%%
%% {@params
%%   {@name To}
%%   {@desc PID of the remote party to synchronize with.}
%% }
%%
%% {@par The caller blocks until an acknowledgement from the remote party is
%%   received. This implementation is inspired by the three-way handshake used
%%   in TCP (see <a
%%   href="https://en.wikipedia.org/wiki/Handshaking#TCP_three-way_handshake">
%%   TCP three-way handshake</a>) but uses Erlang references instead of
%%   incrementing sequence numbers.
%% }
%%
%% {@returns Pair consisting of the remote party PID and synchronization message
%%           reference.
%% }
-spec syn(To :: pid()) -> {pid(), reference()}.
syn(To) when is_pid(To) ->
  Ref = erlang:make_ref(),
  ?TRACE("~p sending syn to ~p (~p) and waiting for syn-ack..", [self(), To, Ref]),
  To ! {'$syn', self(), Ref},
  receive
    {'$syn_ack', Ref} ->
      ?TRACE("~p got syn-ack from ~p (~p)..sending ack.", [self(), To, Ref]),
      To ! {'$ack', Ref},
      {To, Ref}
  end.

%% @doc Replies to a synchronization request, completing the second phase of a
%% three-way handshake.
%%
%% {@params
%%   {@name From}
%%   {@desc PID of the remote party initiating the synchronization.}
%% }
%%
%% {@par The caller blocks until an initiating request is received. The embedded
%%       Erlang reference in the synchronization request is directed back to the
%%       initiating remote party: this assures the latter that the
%%       acknowledgement originates from the correct party participating in the
%%       handshake. In order to ensure that the caller unblocks by one specific
%%       initiating remote party, the PID specified in the argument `From' must
%%       correspond to the PID embedded in the synchronization request message
%%       issued by the initiating remote party. See {@link syn/1}.
%% }
%%
%% {@returns Pair consisting of the initiating remote party PID and
%%           synchronization message reference.
%% }
-spec syn_ack(From :: pid()) -> {pid(), reference()}.
syn_ack(From) when is_pid(From) ->
  ?TRACE("~p waiting for syn from ~p..", [self(), From]),
  receive
    {'$syn', From, Ref} ->
      ?TRACE("~p got syn from ~p (~p)..sending syn-ack..", [self(), From, Ref]),
      From ! {'$syn_ack', Ref},
      receive
        {'$ack', Ref} ->
          ?TRACE("~p ack'ed by ~p (~p).", [self(), From, Ref]),
          {From, Ref}
      end
  end.

%% @doc Executes the specified function asynchronously and returns the result
%% when ready.
%%
%% {@params
%%   {@name Fun}
%%   {@desc Function to execute.}
%% }
%%
%% {@returns Reference identifying the function execution.}
-spec promise(fun()) -> {pid(), reference()}.
promise(Fun) ->
  Self = self(),
  Ref = erlang:make_ref(),
  Pid = spawn(fun() -> Self ! {ok, Ref}, Self ! {promise, Ref, Fun()} end),
  receive
    {ok, Ref} -> {Pid, Ref}
  end.

%% @doc Blocks until the promise identified by the specified reference is
%% fulfilled.
%%
%% {@params
%%   {@name Ref}
%%   {@desc Promise reference.}
%% }
%%
%% {@returns Function execution result.}
-spec then(reference()) -> term().
then(Ref) ->
  receive
    {promise, Ref, Result} ->
      Result
  end.

%% @doc Appends the forward slash character to the specified directory path
%% if this does not exist.
%%
%% {@params
%%   {@name Dir}
%%   {@desc The directory path to append with `/'.}
%% }
%%
%% {@returns The dir name appended with `/'.}
-spec as_dir_name(Dir :: string()) -> Dir0 :: string().
as_dir_name([]) ->
  [$.];
as_dir_name([Char]) when Char =/= $/ ->
  [Char, $/];
as_dir_name(Last = [_]) ->
  Last;
as_dir_name([Char | Name]) ->
  [Char | as_dir_name(Name)].


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------
