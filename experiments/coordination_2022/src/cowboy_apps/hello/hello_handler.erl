%%%-------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%% @copyright (C) 2020 Duncan Paul Attard
%%% @version 0.9
%%%
%%% @doc
%%%
%%% @end
%%% Created: 26. Sep 2020
%%% 
%%% Copyright (c) 2020 Duncan Paul Attard <duncanatt@gmail.com>
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
-module(hello_handler).
-author("Duncan Paul Attard").
-version("0.9").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API exports.
-export([init/2]).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

init(Req0, Opts) ->
  ?TRACE(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Handling request in hello_handler.~n"),

  % Crash or not.
  case bernoulli(0.5) of
    0 -> ok;
    1 -> exit(crash)
  end,

  % HTTP error or not.
  HttpCode =
    case bernoulli(0.5) of
      0 -> 500;
      1 -> 200
    end,

  Req = cowboy_req:reply(HttpCode, #{
    <<"content-type">> => <<"text/plain">>
  }, <<"Hello world!">>, Req0),
  {ok, Req, Opts}.


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

bernoulli(Pr) when is_number(Pr), Pr >= 0, Pr =< 1 ->
  case rand:uniform() of
    U when U =< Pr -> 1;
    _ -> 0
  end.