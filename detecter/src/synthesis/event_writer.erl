%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Event writer that writes trace events to text file as Erlang terms.
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
-module(event_writer).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([start/1]).

%%% Callbacks/Internal.
-export([loop/2]).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Launches the event writer.
%%
%% {@params
%%   {@name File}
%%   {@desc Log file location.}
%% }
%%
%% {@par Log file is opened for writing and previous entries are overwritten.}
%%
%% {@returns Event writer PID.}
start(File) when is_list(File) ->

  % Open log file for writing.
  IoDev = open_writer(File),

  % Launch main loop and register writer process.
  register(?MODULE, Pid = spawn(?MODULE, loop, [IoDev, 0])),
  Pid.


%%% ----------------------------------------------------------------------------
%%% Internal exports.
%%% ----------------------------------------------------------------------------

%% @doc Main server loop
%%
%% {@params
%%   {@name IoDev}
%%   {@desc Main server loop}
%% }
%%
%% {@returns Does not return.}
-spec loop(IoDev :: file:io_device(), EventCnt :: integer()) -> no_return().
loop(IoDev, EventCnt) ->
  receive
    {event, Event} ->
      Chars = unicode:characters_to_binary(
        io_lib:format("~tp.~n", [encode(Event)])
      ),
      io:put_chars(IoDev, Chars),
      loop(IoDev, EventCnt + 1);
    {Pid, stp} ->

      % Handle stop request. Server does not loop again.
      close_writer(IoDev),
      Pid ! {bye, EventCnt}
  end.


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% @private Encodes non-parsable Erlang terms to string.
-spec encode(Term :: term()) -> term().
encode([]) ->
  [];
encode([Hd | Tl]) ->
  [encode(Hd) | encode(Tl)];
encode({A, B}) ->
  {encode(A), encode(B)}; % Tuple done for efficiency.
encode({A, B, C}) ->
  {encode(A), encode(B), encode(C)}; % Triple done for efficiency.
encode(Tuple) when is_tuple(Tuple) ->
  list_to_tuple(encode(tuple_to_list(Tuple)));
encode(Pid) when is_pid(Pid) ->
  erlang:pid_to_list(Pid);
encode(Port) when is_port(Port) ->
  erlang:port_to_list(Port);
encode(Ref) when is_reference(Ref) ->
  erlang:ref_to_list(Ref);
encode(Fun) when is_function(Fun) ->
  erlang:fun_to_list(Fun);
encode(Term) ->
  Term.

%% @private Opens a file for writing.
-spec open_writer(File :: string()) -> file:io_device().
open_writer(File) when is_list(File) ->
  {ok, IoDev} = file:open(File, [write]),
  IoDev.

%% @private Closes and open file.
-spec close_writer(IoDev :: file:io_device()) -> ok | {error, Reason}
  when Reason :: file:posix() | badarg | terminated.
close_writer(IoDev) ->
  file:close(IoDev).