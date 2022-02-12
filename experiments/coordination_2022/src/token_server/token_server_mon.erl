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
-module(token_server_mon).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([mfa_spec/1]).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

mfa_spec({token_server, loop, [_, _]}) ->
  {ok,
    {chs,
      {env, [{str, "+"}]},
      {act,
        {env, [{str, "_@x/{trace, _, spawned, _, {token_server, loop, [OwnTok, _]}}"}, {var, '_@x'}, {pat, {trace, undefined, spawned, undefined, {token_server, loop, [undefined, undefined]}}}]},
        fun({trace, _, spawned, _, {token_server, loop, [OwnTok, _]}}) -> true; (_) -> false end,
        fun({trace, _, spawned, _, {token_server, loop, [OwnTok, _]}}) ->
          {rec,
            {env, [{str, "rec X"}, {var, 'X'}]},
            fun X() ->
              {chs,
                {env, [{str, "+"}]},
                {act,
                  {env, [{str, "_@y/{trace, _, 'receive', {_, _}}"}, {var, '_@y'}, {pat, {trace, undefined, 'receive', {undefined, undefined}}}]},
                  fun({trace, _, 'receive', {_, _}}) -> true; (_) -> false end,
                  fun({trace, _, 'receive', {_, _}}) ->
                    {'and',
                      {env, [{str, "and"}]},
                      {chs,
                        {env, [{str, "+"}]},
                        {act,
                          {env, [{str, "_@z/{trace, _, send, Tok, _} when OwnTok =:= Tok"}, {var, '_@z'}, {pat, {trace, undefined, send, undefined, undefined}}]},
                          fun({trace, _, send, Tok, _}) when OwnTok =:= Tok -> true; (_) -> false end,
                          fun({trace, _, send, Tok, _}) ->
                            {no, {env, [{str, "no"}]}}
                          end
                        },
                        {act,
                          {env, [{str, "NOT(_@z/{trace, _, send, Tok, _} when OwnTok =:= Tok)"}, {var, '_@z'}, {pat, {trace, undefined, send, undefined, undefined}}]},
                          fun({trace, _, send, Tok, _}) when OwnTok =:= Tok -> false; (_) -> true end,
                          fun(_) ->
                            {yes, {env, [{str, "yes"}]}}
                          end
                        }
                      },
                      {chs,
                        {env, [{str, "+"}]},
                        {act,
                          {env, [{str, "_@z/{trace, _, send, Tok, _} when OwnTok =/= Tok"}, {var, '_@z'}, {pat, {trace, undefined, send, undefined, undefined}}]},
                          fun({trace, _, send, Tok, _}) when OwnTok =/= Tok -> true; (_) -> false end,
                          fun({trace, _, send, Tok, _}) ->
                            {var, {env, [{str, "X"}, {var, 'X'}]}, X}
                          end
                        },
                        {act,
                          {env, [{str, "NOT(_@z/{trace, _, send, Tok, _} when OwnTok =/= Tok)"}, {var, '_@z'}, {pat, {trace, undefined, send, undefined, undefined}}]},
                          fun({trace, _, send, Tok, _}) when OwnTok =/= Tok -> false; (_) -> true end,
                          fun(_) ->
                            {yes, {env, [{str, "yes"}]}}
                          end
                        }
                      }
                    }
                  end
                },
                {act,
                  {env, [{str, "NOT(_@y/{trace, _, 'receive', {_, _}})"}, {var, '_@y'}, {pat, {trace, undefined, 'receive', {undefined, undefined}}}]},
                  fun({trace, _, 'receive', {_, _}}) -> false; (_) -> true end,
                  fun(_) ->
                    {yes, {env, [{str, "yes"}]}}
                  end
                }
              }
            end
          }
        end
      },
      {act,
        {env, [{str, "NOT(_@x/{trace, _, spawned, _, {token_server, loop, [OwnTok, _]}})"}, {var, '_@x'}, {pat, {trace, undefined, spawned, undefined, {token_server, loop, [undefined, undefined]}}}]},
        fun({trace, _, spawned, _, {token_server, loop, [OwnTok, _]}}) -> false; (_) -> true end,
        fun(_) ->
          {yes, {env, [{str, "yes"}]}}
        end
      }
    }
  };
mfa_spec(_) ->
  undefined.

%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------
