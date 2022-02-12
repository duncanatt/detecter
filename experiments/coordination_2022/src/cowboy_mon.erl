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
-module(cowboy_mon).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
%%-include("log.hrl").

%%% Public API.
-export([mfa_spec/1]).

%%% Callbacks/Internal.
-export([]).

%%% Types.
-export_type([]).

%%% Implemented behaviors.
%-behavior().


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

% Property on Ranch Acceptor: "Acceptor cannot receive a rouge message when it
% start the protocol handler."
%
% [_Acc <- _AccSup, ranch_acceptor:loop(_, _, _, _, _)]
% max X(
%   [Acc:_ConnSup ! {ranch_conns_sup, start_protocol, Acc0, _Port} when Acc =:= Acc0][_Acc ? _ConnSup] X
%   and
%   [Acc:_ConnSup ! {ranch_conns_sup, start_protocol, Acc0, _Port} when Acc =/= Acc0]ff)
% )
%%mfa_spec({ranch_acceptor, loop, [_, _, _, _, _]}) ->
%%  ok;

% Property on Ranch Request Process: "Request process always returns HTTP 200
% OK."
%
% [_ReqProc <- _ConnProc, cowboy_stream_h:request_process(_, _, _])]
% max X(
%   [_ReqProc:_Sock ! {{_ConnProc, _}, {response, HttpCode, _, _}} when HttpCode < 500] X
%   and
%   [_ReqProc:_Sock ! {{_ConnProc, _}, {response, HttpCode, _, _}} when HttpCode >= 500] ff
% )
%%mfa_spec({cowboy_stream_h, request_process, [_, _, _]}) ->
%%  {ok,
%%    {chs,
%%      {env, [{str, "+"}]},
%%      {act,
%%        {env, [{str, "_@A/{trace, _ ,spawned, _, {cowboy_stream_h, request_process, [_, _, _]}}"}, {var, '_@A'}, {pat, {trace, undefined, spawned, undefined, {cowboy_stream_h, request_process, [undefined, undefined, undefined]}}}]},
%%        fun({trace, _, spawned, _, {cowboy_stream_h, request_process, [_, _, _]}}) -> true; (_) -> false end,
%%        fun({trace, _, spawned, _, {cowboy_stream_h, request_process, [_, _, _]}}) ->
%%          {rec,
%%            {env, [{str, "rec X"}, {var, 'X'}]},
%%            fun X() ->
%%              {'and',
%%                {env, [{str, "and"}]},
%%                {chs,
%%                  {env, [{str, "+"}]},
%%                  {act,
%%                    {env, [{str, "_@B/{trace, _ , send, {{_ConnProc, _}, {response, HttpCode, _, _}}, _} when HttpCode < 500"}, {var, '_@B'}, {pat, {trace, undefined, send, {{undefined, undefined}, {response, undefined, undefined, undefined}}, undefined}}]},
%%                    fun({trace, _, send, {{_ConnProc, _}, {response, HttpCode, _, _}}, _}) when HttpCode < 500 ->
%%                      true; (_) -> false end,
%%                    fun({trace, _, send, {{_ConnProc, _}, {response, HttpCode, _, _}}, _}) ->
%%                      {var, {env, [{str, "X"}, {var, 'X'}]}, X}
%%                    end
%%                  },
%%                  {act,
%%                    {env, [{str, "not(_@B/{trace, _ , send, {{_ConnProc, _}, {response, HttpCode, _, _}}, _} when HttpCode < 500)"}, {var, '_@B'}, {pat, {trace, undefined, send, {{undefined, undefined}, {response, undefined, undefined, undefined}}, undefined}}]},
%%                    fun({trace, _, send, {{_ConnProc, _}, {response, HttpCode, _, _}}, _}) when HttpCode < 500 ->
%%                      false; (_) -> true end,
%%                    fun(_) ->
%%                      {yes, {env, [{str, "yes"}]}}
%%                    end
%%                  }
%%                },
%%                {chs,
%%                  {env, [{str, "+"}]},
%%                  {act,
%%                    {env, [{str, "_@B/{trace, _ , send, {{_ConnProc, _}, {response, HttpCode, _, _}}, _} when HttpCode >= 500"}, {var, '_@B'}, {pat, {trace, undefined, send, {{undefined, undefined}, {response, undefined, undefined, undefined}}, undefined}}]},
%%                    fun({trace, _, send, {{_ConnProc, _}, {response, HttpCode, _, _}}, _}) when HttpCode >= 500 ->
%%                      true; (_) -> false end,
%%                    fun({trace, _, send, {{_ConnProc, _}, {response, HttpCode, _, _}}, _}) ->
%%                      {no, {env, [{str, "no"}]}}
%%                    end
%%                  },
%%                  {act,
%%                    {env, [{str, "not(_@B/{trace, _ , send, {{_ConnProc, _}, {response, HttpCode, _, _}}, _} when HttpCode >= 500)"}, {var, '_@B'}, {pat, {trace, undefined, send, {{undefined, undefined}, {response, undefined, undefined, undefined}}, undefined}}]},
%%                    fun({trace, _, send, {{_ConnProc, _}, {response, HttpCode, _, _}}, _}) when HttpCode >= 500 ->
%%                      false; (_) -> true end,
%%                    fun(_) ->
%%                      {yes, {env, [{str, "yes"}]}}
%%                    end
%%                  }
%%                }
%%              }
%%            end
%%          }
%%        end
%%      },
%%      {act,
%%        {env, [{str, "not(_@A/{trace, _ ,spawned, _, {cowboy_stream_h, request_process, [_, _, _]}})"}, {var, '_@A'}, {pat, {trace, undefined, spawned, undefined, {cowboy_stream_h, request_process, [undefined, undefined, undefined]}}}]},
%%        fun({trace, _, spawned, _, {cowboy_stream_h, request_process, [_, _, _]}}) -> false; (_) -> true end,
%%        fun(_) ->
%%          {yes, {env, [{str, "yes"}]}}
%%        end
%%      }
%%    }
%%  };

% Property on Ranch Connection Process: "Connection Process is created correctly
% and in turn, the Request Process executes correctly and terminates with a
% normal exit reason."
%
% [_ConnProc <- _ConnSup, cowboy_clear:connection_process(_, _, _, _)]
% max X(
%   [_ConnProc ? {handshake, http, ranch_tcp, _Port, _Timeout}] [_ConnProc ? {tcp, _, _}]
%   [_ConnProc -> _ReqProc, cowboy_stream_h:request_process(_, _, _)] (
%     [_ConnProc ? {{_ConnProc, _}, {response, HttpCode, _, _}} when HttpCode < 500] (
%       [_ConnProc ? {'EXIT', _, normal}] [_ConnProc ? {tcp_closed, _}] X
%       and
%       [_ConnProc ? {'EXIT', _, crash}] ff
%     )
%     and
%     [_ConnProc ? {{_ConnProc, _}, {response, HttpCode, _, _}} when HttpCode >= 500] ff
%   )
% )
%
%%mfa_spec({cowboy_clear, connection_process, [_, _, _, _]}) ->
%%  {ok,
%%    {chs,
%%      {env, [{str, "+"}]},
%%      {act,
%%        {env, [{str, "_@A/{trace, _, spawned, _, {cowboy_clear, connection_process, [_, _, _, _]}}"}, {var, '_@A'}, {pat, {trace, undefined, spawned, undefined, {cowboy_clear, connection_process, [undefined, undefined, undefined, undefined]}}}]},
%%        fun({trace, _, spawned, _, {cowboy_clear, connection_process, [_, _, _, _]}}) -> true; (_) -> false end,
%%        fun({trace, _, spawned, _, {cowboy_clear, connection_process, [_, _, _, _]}}) ->
%%          {rec,
%%            {env, [{str, "rec X"}, {var, 'X'}]},
%%            fun X() ->
%%              {chs,
%%                {env, [{str, "+"}]},
%%                {act,
%%                  {env, [{str, "_@B/{trace, _, 'receive', {handshake, _, ranch_tcp, _Port, _Timeout}}"}, {var, '_@B'}, {pat, {trace, undefined, 'receive', {handshake, http, ranch_tcp, undefined, undefined}}}]},
%%                  fun({trace, _, 'receive', {handshake, _, ranch_tcp, _Port, _Timeout}}) -> true; (_) -> false end,
%%                  fun({trace, _, 'receive', {handshake, _, ranch_tcp, _Port, _Timeout}}) ->
%%                    {chs,
%%                      {env, [{str, "+"}]},
%%                      {act,
%%                        {env, [{str, "_@C/{trace, _, 'receive', {tcp, _, _}}"}, {var, '_@C'}, {pat, {trace, undefined, 'receive', {tcp, undefined, undefined}}}]},
%%                        fun({trace, _, 'receive', {tcp, _, _}}) -> true; (_) -> false end,
%%                        fun({trace, _, 'receive', {tcp, _, _}}) ->
%%                          {chs,
%%                            {env, [{str, "+"}]},
%%                            {act,
%%                              {env, [{str, "_@D/{trace, _, spawn, _, {cowboy_stream_h, request_process, [_, _, _]}}"}, {var, '_@D'}, {pat, {trace, undefined, spawn, undefined, {cowboy_stream_h, request_process, [undefined, undefined, undefined]}}}]},
%%                              fun({trace, _, spawn, _, {cowboy_stream_h, request_process, [_, _, _]}}) -> true; (_) ->
%%                                false end,
%%                              fun({trace, _, spawn, _, {cowboy_stream_h, request_process, [_, _, _]}}) ->
%%                                {'and',
%%                                  {env, [{str, "and"}]},
%%                                  {chs,
%%                                    {env, [{str, "+"}]},
%%                                    {act,
%%                                      {env, [{str, "_@E/{trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}} when HttpCode < 500"}, {var, '_@E'}, {pat, {trace, undefined, 'receive', {{undefined, undefined}, {response, undefined, undefined, undefined}}}}]},
%%                                      fun({trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}}) when HttpCode < 500 -> true; (_) -> false end,
%%                                      fun({trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}}) ->
%%                                        {'and',
%%                                          {env, [{str, "and"}]},
%%                                          {chs,
%%                                            {env, [{str, "+"}]},
%%                                            {act,
%%                                              {env, [{str, "_@F/{trace, _, 'receive', {'EXIT', _, normal}}"}, {var, '_@F'}, {pat, {trace, undefined, 'receive', {'EXIT', undefined, normal}}}]},
%%                                              fun({trace, _, 'receive', {'EXIT', _, normal}}) -> true; (_) -> false end,
%%                                              fun({trace, _, 'receive', {'EXIT', _, normal}}) ->
%%                                                {chs,
%%                                                  {env, [{str, "+"}]},
%%                                                  {act,
%%                                                    {env, [{str, "_@G/{trace, _, 'receive', {tcp_closed, _}}"}, {var, '_@G'}, {pat, {trace, undefined, 'receive', {tcp_closed, undefined}}}]},
%%                                                    fun({trace, _, 'receive', {tcp_closed, _}}) -> true; (_) -> false end,
%%                                                    fun({trace, _, 'receive', {tcp_closed, _}}) ->
%%                                                      {var, {env, [{str, "X"}, {var, 'X'}]}, X}
%%                                                    end
%%                                                  },
%%                                                  {act,
%%                                                    {env, [{str, "not(_@G/{trace, _, 'receive', {tcp_closed, _}})"}, {var, '_@G'}, {pat, {trace, undefined, 'receive', {tcp_closed, undefined}}}]},
%%                                                    fun({trace, _, 'receive', {tcp_closed, _}}) -> false; (_) -> true end,
%%                                                    fun(_) ->
%%                                                      {yes, {env, [{str, "yes"}]}}
%%                                                    end
%%                                                  }
%%                                                }
%%                                              end
%%                                            },
%%                                            {act,
%%                                              {env, [{str, "not(_@F/{trace, _, 'receive', {'EXIT', _, normal}})"}, {var, '_@F'}, {pat, {trace, undefined, 'receive', {'EXIT', undefined, normal}}}]},
%%                                              fun({trace, _, 'receive', {'EXIT', _, normal}}) -> false; (_) -> true end,
%%                                              fun(_) ->
%%                                                {yes, {env, [{str, "yes"}]}}
%%                                              end
%%                                            }
%%                                          },
%%                                          {chs,
%%                                            {env, [{str, "+"}]},
%%                                            {act,
%%                                              {env, [{str, "_@F/{trace, _, 'receive', {'EXIT', _, crash}}"}, {var, '_@F'}, {pat, {trace, undefined, 'receive', {'EXIT', undefined, crash}}}]},
%%                                              fun({trace, _, 'receive', {'EXIT', _, crash}}) -> true; (_) -> false end,
%%                                              fun({trace, _, 'receive', {'EXIT', _, crash}}) ->
%%                                                {no, {env, [{str, "no"}]}}
%%                                              end
%%                                            },
%%                                            {act,
%%                                              {env, [{str, "not(_@F/{trace, _, 'receive', {'EXIT', _, crash}})"}, {var, '_@F'}, {pat, {trace, undefined, 'receive', {'EXIT', undefined, crash}}}]},
%%                                              fun({trace, _, 'receive', {'EXIT', _, crash}}) -> false; (_) -> true end,
%%                                              fun(_) ->
%%                                                {yes, {env, [{str, "yes"}]}}
%%                                              end
%%                                            }
%%                                          }
%%                                        }
%%                                      end
%%                                    },
%%                                    {act,
%%                                      {env, [{str, "not(_@E/{trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}} when HttpCode < 500)"}, {var, '_@E'}, {pat, {trace, undefined, 'receive', {{undefined, undefined}, {response, undefined, undefined, undefined}}}}]},
%%                                      fun({trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}}) when HttpCode < 500 -> false; (_) -> true end,
%%                                      fun(_) ->
%%                                        {yes, {env, [{str, "yes"}]}}
%%                                      end
%%                                    }
%%                                  },
%%                                  {chs,
%%                                    {env, [{str, "+"}]},
%%                                    {act,
%%                                      {env, [{str, "_@E/{trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}} when HttpCode >= 500"}, {var, '_@E'}, {pat, {trace, undefined, 'receive', {{undefined, undefined}, {response, undefined, undefined, undefined}}}}]},
%%                                      fun({trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}}) when HttpCode >= 500 -> true; (_) -> false end,
%%                                      fun({trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}}) ->
%%                                        {no, {env, [{str, "no"}]}}
%%                                      end
%%                                    },
%%                                    {act,
%%                                      {env, [{str, "not(_@E/{trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}} when HttpCode >= 500)"}, {var, '_@E'}, {pat, {trace, undefined, 'receive', {{undefined, undefined}, {response, undefined, undefined, undefined}}}}]},
%%                                      fun({trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}}) when HttpCode >= 500 -> false; (_) -> true end,
%%                                      fun(_) ->
%%                                        {yes, {env, [{str, "yes"}]}}
%%                                      end
%%                                    }
%%                                  }
%%                                }
%%                              end
%%                            },
%%                            {act,
%%                              {env, [{str, "not(_@D/{trace, _, spawn, _, {cowboy_stream_h, request_process, [_, _, _]}})"}, {var, '_@D'}, {pat, {trace, undefined, spawn, undefined, {cowboy_stream_h, request_process, [undefined, undefined, undefined]}}}]},
%%                              fun({trace, _, spawn, _, {cowboy_stream_h, request_process, [_, _, _]}}) -> false; (_) ->
%%                                true end,
%%                              fun(_) ->
%%                                {yes, {env, [{str, "yes"}]}}
%%                              end
%%                            }
%%                          }
%%                        end
%%                      },
%%                      {act,
%%                        {env, [{str, "not(_@C/{trace, _, 'receive', {tcp, _, _}})"}, {var, '_@C'}, {pat, {trace, undefined, 'receive', {tcp, undefined, undefined}}}]},
%%                        fun({trace, _, 'receive', {tcp, _, _}}) -> false; (_) -> true end,
%%                        fun(_) ->
%%                          {yes, {env, [{str, "yes"}]}}
%%                        end
%%                      }
%%                    }
%%                  end
%%                },
%%                {act,
%%                  {env, [{str, "not(_@B/{trace, _, 'receive', {handshake, _, ranch_tcp, _Port, _Timeout}})"}, {var, '_@B'}, {pat, {trace, undefined, 'receive', {handshake, http, ranch_tcp, undefined, undefined}}}]},
%%                  fun({trace, _, 'receive', {handshake, _, ranch_tcp, _Port, _Timeout}}) -> false; (_) -> true end,
%%                  fun(_) ->
%%                    {yes, {env, [{str, "yes"}]}}
%%                  end
%%                }
%%              }
%%            end
%%          }
%%        end
%%      },
%%      {act,
%%        {env, [{str, "not(_@A/{trace, _, spawned, _, {cowboy_clear, connection_process, [_, _, _, _]}})"}, {var, '_@A'}, {pat, {trace, undefined, spawned, undefined, {cowboy_clear, connection_process, [undefined, undefined, undefined, undefined]}}}]},
%%        fun({trace, _, spawned, _, {cowboy_clear, connection_process, [_, _, _, _]}}) -> false; (_) -> true end,
%%        fun(_) ->
%%          {yes, {env, [{str, "yes"}]}}
%%        end
%%      }
%%    }
%%  };

% [_ConnProc <- _ConnSup, cowboy_clear:connection_process(_, _, _, _)]
% max X(
%   [_ConnProc ? {handshake, http, ranch_tcp, _Port, _Timeout}] [_ConnProc ? {tcp, _, _}]
%   [_ConnProc -> _ReqProc, cowboy_stream_h:request_process(_, _, _)] (
%     [_ConnProc ? {{_ConnProc, _}, {response, HttpCode, _, _}} when HttpCode < 500] (
%       [_ConnProc ? {'EXIT', _, normal}] [_ConnProc ? _] X
%       and
%       [_ConnProc ? {'EXIT', _, crash}] ff
%     )
%     and
%     [_ConnProc ? {{_ConnProc, _}, {response, HttpCode, _, _}} when HttpCode >= 500] ff
%     and
%     [_ConnProc ? {'EXIT', _, {crash, _StackTrace}}] ff
%   )
% )
mfa_spec({cowboy_clear, connection_process, [_, _, _, _]}) ->
  {ok,
    {chs,
      {env, [{str, "+"}]},
      {act,
        {env, [{str, "_@A/{trace, _, spawned, _, {cowboy_clear, connection_process, [_, _, _, _]}}"}, {var, '_@A'}, {pat, {trace, undefined, spawned, undefined, {cowboy_clear, connection_process, [undefined, undefined, undefined, undefined]}}}]},
        fun({trace, _, spawned, _, {cowboy_clear, connection_process, [_, _, _, _]}}) -> true; (_) -> false end,
        fun({trace, _, spawned, _, {cowboy_clear, connection_process, [_, _, _, _]}}) ->
          {rec,
            {env, [{str, "rec X"}, {var, 'X'}]},
            fun X() ->
              {chs,
                {env, [{str, "+"}]},
                {act,
                  {env, [{str, "_@B/{trace, _, 'receive', {handshake, _, ranch_tcp, _Port, _Timeout}}"}, {var, '_@B'}, {pat, {trace, undefined, 'receive', {handshake, http, ranch_tcp, undefined, undefined}}}]},
                  fun({trace, _, 'receive', {handshake, _, ranch_tcp, _Port, _Timeout}}) -> true; (_) -> false end,
                  fun({trace, _, 'receive', {handshake, _, ranch_tcp, _Port, _Timeout}}) ->
                    {chs,
                      {env, [{str, "+"}]},
                      {act,
                        {env, [{str, "_@C/{trace, _, 'receive', {tcp, _, _}}"}, {var, '_@C'}, {pat, {trace, undefined, 'receive', {tcp, undefined, undefined}}}]},
                        fun({trace, _, 'receive', {tcp, _, _}}) -> true; (_) -> false end,
                        fun({trace, _, 'receive', {tcp, _, _}}) ->
                          {chs,
                            {env, [{str, "+"}]},
                            {act,
                              {env, [{str, "_@D/{trace, _, spawn, _, {cowboy_stream_h, request_process, [_, _, _]}}"}, {var, '_@D'}, {pat, {trace, undefined, spawn, undefined, {cowboy_stream_h, request_process, [undefined, undefined, undefined]}}}]},
                              fun({trace, _, spawn, _, {cowboy_stream_h, request_process, [_, _, _]}}) -> true; (_) ->
                                false end,
                              fun({trace, _, spawn, _, {cowboy_stream_h, request_process, [_, _, _]}}) ->
                                {'and',
                                  {env, [{str, "and"}]},
                                  {'and',
                                    {env, [{str, "and"}]},
                                    {chs,
                                      {env, [{str, "+"}]},
                                      {act,
                                        {env, [{str, "_@E/{trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}} when HttpCode < 500"}, {var, '_@E'}, {pat, {trace, undefined, 'receive', {{undefined, undefined}, {response, undefined, undefined, undefined}}}}]},
                                        fun({trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}}) when HttpCode < 500 -> true; (_) -> false end,
                                        fun({trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}}) ->
                                          {'and',
                                            {env, [{str, "and"}]},
                                            {chs,
                                              {env, [{str, "+"}]},
                                              {act,
                                                {env, [{str, "_@F/{trace, _, 'receive', {'EXIT', _, normal}}"}, {var, '_@F'}, {pat, {trace, undefined, 'receive', {'EXIT', undefined, normal}}}]},
                                                fun({trace, _, 'receive', {'EXIT', _, normal}}) -> true; (_) -> false end,
                                                fun({trace, _, 'receive', {'EXIT', _, normal}}) ->
                                                  {chs,
                                                    {env, [{str, "+"}]},
                                                    {act,
                                                      {env, [{str, "_@G/{trace, _, 'receive', _}"}, {var, '_@G'}, {pat, {trace, undefined, 'receive', undefined}}]},
                                                      fun({trace, _, 'receive', _}) -> true; (_) -> false end,
                                                      fun({trace, _, 'receive', _}) ->
                                                        {var, {env, [{str, "X"}, {var, 'X'}]}, X}
                                                      end
                                                    },
                                                    {act,
                                                      {env, [{str, "not(_@G/{trace, _, 'receive', _})"}, {var, '_@G'}, {pat, {trace, undefined, 'receive', undefined}}]},
                                                      fun({trace, _, 'receive', _}) -> false; (_) -> true end,
                                                      fun(_) ->
                                                        {yes, {env, [{str, "yes"}]}}
                                                      end
                                                    }
                                                  }
                                                end
                                              },
                                              {act,
                                                {env, [{str, "not(_@F/{trace, _, 'receive', {'EXIT', _, normal}})"}, {var, '_@F'}, {pat, {trace, undefined, 'receive', {'EXIT', undefined, normal}}}]},
                                                fun({trace, _, 'receive', {'EXIT', _, normal}}) -> false; (_) -> true end,
                                                fun(_) ->
                                                  {yes, {env, [{str, "yes"}]}}
                                                end
                                              }
                                            },
                                            {chs,
                                              {env, [{str, "+"}]},
                                              {act,
                                                {env, [{str, "_@F/{trace, _, 'receive', {'EXIT', _, crash}}"}, {var, '_@F'}, {pat, {trace, undefined, 'receive', {'EXIT', undefined, crash}}}]},
                                                fun({trace, _, 'receive', {'EXIT', _, crash}}) -> true; (_) -> false end,
                                                fun({trace, _, 'receive', {'EXIT', _, crash}}) ->
                                                  {no, {env, [{str, "no"}]}}
                                                end
                                              },
                                              {act,
                                                {env, [{str, "not(_@F/{trace, _, 'receive', {'EXIT', _, crash}})"}, {var, '_@F'}, {pat, {trace, undefined, 'receive', {'EXIT', undefined, crash}}}]},
                                                fun({trace, _, 'receive', {'EXIT', _, crash}}) -> false; (_) -> true end,
                                                fun(_) ->
                                                  {yes, {env, [{str, "yes"}]}}
                                                end
                                              }
                                            }
                                          }
                                        end
                                      },
                                      {act,
                                        {env, [{str, "not(_@E/{trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}} when HttpCode < 500)"}, {var, '_@E'}, {pat, {trace, undefined, 'receive', {{undefined, undefined}, {response, undefined, undefined, undefined}}}}]},
                                        fun({trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}}) when HttpCode < 500 -> false; (_) -> true end,
                                        fun(_) ->
                                          {yes, {env, [{str, "yes"}]}}
                                        end
                                      }
                                    },
                                    {chs,
                                      {env, [{str, "+"}]},
                                      {act,
                                        {env, [{str, "_@E/{trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}} when HttpCode >= 500"}, {var, '_@E'}, {pat, {trace, undefined, 'receive', {{undefined, undefined}, {response, undefined, undefined, undefined}}}}]},
                                        fun({trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}}) when HttpCode >= 500 -> true; (_) -> false end,
                                        fun({trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}}) ->
                                          {no, {env, [{str, "no"}]}}
                                        end
                                      },
                                      {act,
                                        {env, [{str, "not(_@E/{trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}} when HttpCode >= 500)"}, {var, '_@E'}, {pat, {trace, undefined, 'receive', {{undefined, undefined}, {response, undefined, undefined, undefined}}}}]},
                                        fun({trace, _, 'receive', {{_ConnProc, _}, {response, HttpCode, _, _}}}) when HttpCode >= 500 -> false; (_) -> true end,
                                        fun(_) ->
                                          {yes, {env, [{str, "yes"}]}}
                                        end
                                      }
                                    }
                                  },
                                  {chs,
                                    {env, [{str, "+"}]},
                                    {act,
                                      {env, [{str, "_@E/{trace, _, 'receive', {'EXIT', _, {crash, _StackTrace}}}"}, {var, '_@E'}, {pat, {trace, undefined, 'receive', {'EXIT', undefined, {crash, undefined}}}}]},
                                      fun({trace, _, 'receive', {'EXIT', _, {crash, _StackTrace}}}) -> true; (_) -> false end,
                                      fun({trace, _, 'receive', {'EXIT', _, {crash, _StackTrace}}}) ->
                                        {no, {env, [{str, "no"}]}}
                                      end
                                    },
                                    {act,
                                      {env, [{str, "not(_@E/{trace, _, 'receive', {'EXIT', _, {crash, _StackTrace}}})"}, {var, '_@E'}, {pat, {trace, undefined, 'receive', {'EXIT', undefined, undefined}}}]},
                                      fun({trace, _, 'receive', {'EXIT', _, {crash, _StackTrace}}}) -> false; (_) -> true end,
                                      fun(_) ->
                                        {yes, {env, [{str, "yes"}]}}
                                      end
                                    }
                                  }
                                }
                              end
                            },
                            {act,
                              {env, [{str, "not(_@D/{trace, _, spawn, _, {cowboy_stream_h, request_process, [_, _, _]}})"}, {var, '_@D'}, {pat, {trace, undefined, spawn, undefined, {cowboy_stream_h, request_process, [undefined, undefined, undefined]}}}]},
                              fun({trace, _, spawn, _, {cowboy_stream_h, request_process, [_, _, _]}}) -> false; (_) ->
                                true end,
                              fun(_) ->
                                {yes, {env, [{str, "yes"}]}}
                              end
                            }
                          }
                        end
                      },
                      {act,
                        {env, [{str, "not(_@C/{trace, _, 'receive', {tcp, _, _}})"}, {var, '_@C'}, {pat, {trace, undefined, 'receive', {tcp, undefined, undefined}}}]},
                        fun({trace, _, 'receive', {tcp, _, _}}) -> false; (_) -> true end,
                        fun(_) ->
                          {yes, {env, [{str, "yes"}]}}
                        end
                      }
                    }
                  end
                },
                {act,
                  {env, [{str, "not(_@B/{trace, _, 'receive', {handshake, _, ranch_tcp, _Port, _Timeout}})"}, {var, '_@B'}, {pat, {trace, undefined, 'receive', {handshake, http, ranch_tcp, undefined, undefined}}}]},
                  fun({trace, _, 'receive', {handshake, _, ranch_tcp, _Port, _Timeout}}) -> false; (_) -> true end,
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
        {env, [{str, "not(_@A/{trace, _, spawned, _, {cowboy_clear, connection_process, [_, _, _, _]}})"}, {var, '_@A'}, {pat, {trace, undefined, spawned, undefined, {cowboy_clear, connection_process, [undefined, undefined, undefined, undefined]}}}]},
        fun({trace, _, spawned, _, {cowboy_clear, connection_process, [_, _, _, _]}}) -> false; (_) -> true end,
        fun(_) ->
          {yes, {env, [{str, "yes"}]}}
        end
      }
    }
  };

% Undefined.
mfa_spec(_) ->
  undefined.

%%{trace, undefined, spawned, undefined, {undefined, undefined, [undefined, undefined, undefined]}}
%%{trace, undefined, spawned, undefined, {undefined, undefined, [undefined, undefined, undefined]}}
%%{trace,<0.129.0>,'receive',{'EXIT',<0.130.0>,
%%{
%%  badarith,
%%  [{hello_handler,init,2,[{file,[115,114,99,47,99,111,119,98,111,121,95,97,112,112,115,47,104,101,108,108,111,47,104,101,108,108,111,95,104,97,110,100,108,101,114,46,101,114,108]},{line,66}]},{cowboy_handler,execute,2,[{file,[115,114,99,47,99,111,119,98,111,121,47,99,111,119,98,111,121,95,104,97,110,100,108,101,114,46,101,114,108]},{line,40}]},{cowboy_stream_h,execute,3,[{file,[115,114,99,47,99,111,119,98,111,121,47,99,111,119,98,111,121,95,115,116,114,101,97,109,95,104,46,101,114,108]},{line,306}]},{cowboy_stream_h,request_process,3,[{file,[115,114,99,47,99,111,119,98,111,121,47,99,111,119,98,111,121,95,115,116,114,101,97,109,95,104,46,101,114,108]},{line,296}]},{proc_lib,init_p,3,[{file,[112,114,111,99,95,108,105,98,46,101,114,108]},{line,211}]}]
%%}
%%}}

% Umm..now write some nice code.. ^_^

%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------
