%%%-------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%% @copyright (C) 2019 Duncan Paul Attard
%%% @version 0.9
%%%
%%% @doc
%%%
%%% @end
%%% Created: 16. Sep 2019
%%% 
%%% Copyright (c) 2019 Duncan Paul Attard <duncanatt@gmail.com>
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------
-module(gen_file_poller_impl).
-author("Duncan Paul Attard").
-version("0.9").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").

%%% Public API exports.
-export([start/1, stop/0]).

%%% Callbacks.
-export([init/1, handle_line/3, terminate/2]).

%%% Implemented behaviors.
-behavior(gen_file_poller).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------
-spec start(File :: string()) -> no_return().
start(File) ->
  gen_file_poller:start({local, ?MODULE}, ?MODULE, File, []).

%%start(File, Name, Opts) ->
%%  gen_file_poller:start(Name, ?MODULE, File, Opts).

-spec stop() -> no_return().
stop() ->
  gen_file_poller:stop(?MODULE).

%%% ----------------------------------------------------------------------------
%%% Callbacks.
%%% ----------------------------------------------------------------------------

init(Args) ->
  io:format("Args=~p~n", [Args]),
  {ok, {lines_read, 0}}.

-spec handle_line(Line, LineInfo, State) ->
  {ok, NewState :: term()} when
  Line :: [byte()],
  LineInfo :: gen_file_poller:line_info(),
  State :: term().
handle_line(Line, _, {lines_read, Cnt}) ->
  io:format("Line is ~p~n", [Line]),
  {ok, {lines_read, Cnt + 1}}.

terminate(Reason, State) ->
  io:format("Terminating with Reason=~p, State=~p~n", [Reason, State]).