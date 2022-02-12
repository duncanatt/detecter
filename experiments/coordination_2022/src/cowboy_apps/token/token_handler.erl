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
-module(token_handler).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([init/2]).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

init(Req, Opts) ->

  % Get HTTP method.
  HttpMethod = cowboy_req:method(Req),

  % Get request parameters from query string.
  Params = cowboy_req:match_qs([{type, [], undefined}], Req),

  % Handle request and formulate response.
  {HttpCode, Resp} = handle(HttpMethod, Params),
  HttpResp = cowboy_req:reply(HttpCode, #{
    <<"content-type">> => <<"application/json; charset=utf-8">>
  }, Resp, Req),

  {ok, HttpResp, Opts}.


%%% ----------------------------------------------------------------------------
%%% Private request handling functions.
%%% ----------------------------------------------------------------------------

handle(<<"GET">>, #{type := <<"uuid">>}) ->

  % Generate token as UUID and send JSON response.
  Resp = io_lib:format("{\"id\":\"~s\"}", [uuid()]),
  {200, iolist_to_binary(Resp)};

handle(<<"GET">>, #{type := <<"tiny">>}) ->

  % Generate token as tiny ID and send JSON response.
  Resp = io_lib:format("{\"id\":\"~s\"}", [tiny()]),
  {200, iolist_to_binary(Resp)};

handle(<<"GET">>, #{type := undefined}) ->

  % Incomplete request: token type not specified.
  {400, <<"{\"error\": \"Missing 'type' parameter in request\"}">>};

handle(<<"GET">>, #{type := Type}) ->

  % Invalid request: token type unknown.
  Resp = io_lib:format("{\"error\": \"Unknown token type '~s'\"}", [Type]),
  {500, iolist_to_binary(Resp)};

handle(_, _) ->

  % Unsupported HTTP method.
  exit(bad_http_method).


%%% ----------------------------------------------------------------------------
%%% Private ID generating functions.
%%% ----------------------------------------------------------------------------

uuid() ->
  <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
  Str = io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
    [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E]),
  list_to_binary(Str).

tiny() ->

  % Generate data input to hash based on millis timestamp and a uniform random
  % number capped by the millis.
  Data = [rand:uniform(erlang:system_time())],

  % Create new salt for hash ID using the PID, and encode the data.
  Ctx = hashids:new([{salt, pid_to_list(self())}]),
  list_to_binary(hashids:encode(Ctx, Data)).
