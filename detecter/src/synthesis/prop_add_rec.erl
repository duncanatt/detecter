-module(prop_add_rec).
-author("detectEr").
-generated("2022/ 1/06 11:08:42").
-export([mfa_spec/1]).

-include("log.hrl").

% Property that first action is unique, but for only numbers and does not apply to a real program, but for a simple dummy demo.
% But it is a blueprint for the synthesis.
%%mfa_spec({calc_server, loop, [_]}) ->
%%  {ok,
%%    {chs,
%%      {env, [{str, "+"}]},
%%      {act,
%%        {env, [{str, "{:A} when true"}, {var, 'A'}, {pat, undefined}]},
%%        fun(A) -> true; (_) -> false end,
%%        fun(A) ->
%%          {rec,
%%            {env, [{str, "rec X"}, {var, 'X'}]},
%%            fun X() ->
%%              {'and',
%%                {env, [{str, "and"}]},
%%                {chs,
%%                  {env, [{str, "+"}]},
%%                  {act,
%%                    {env, [{str, "{:B} when {:A}=:={:B}"}, {var, 'B'}, {pat, undefined}]},
%%                    fun(B) when A =:= B -> true; (_) -> false end,
%%                    fun(B) ->
%%                      {no, {env, [{str, "no"}]}}
%%                    end
%%                  },
%%                  {act,
%%                    {env, [{str, "{:B} when not({:A}=:={:B})"}, {var, 'B'}, {pat, undefined}]},
%%                    fun(B) when A =:= B -> false; (_) -> true end,
%%                    fun(B) ->
%%                      {yes, {env, [{str, "yes"}]}}
%%                    end
%%                  }
%%                },
%%                {chs,
%%                  {env, [{str, "+"}]},
%%                  {act,
%%                    {env, [{str, "{:B} when {:A}=/={:B}"}, {var, 'B'}, {pat, undefined}]},
%%                    fun(B) when A =/= B -> true; (_) -> false end,
%%                    fun(B) ->
%%                      {var, {env, [{str, "X"}, {var, 'X'}]}, X}
%%                    end
%%                  },
%%                  {act,
%%                    {env, [{str, "{:B} when not({:A}=/={:B})"}, {var, 'B'}, {pat, undefined}]},
%%                    fun(B) when A =/= B -> false; (_) -> true end,
%%                    fun(B) ->
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
%%        {env, [{str, "{:A} when not(true)"}, {var, 'A'}, {pat, undefined}]},
%%        fun(A) -> false; (_) -> true end,
%%        fun(A) ->
%%          {yes, {env, [{str, "yes"}]}}
%%        end
%%      }
%%    }
%%  };
%%mfa_spec(_) ->
%%  undefined.


% This could be a security property, where the secret that we started with cannot
% be sent to the clients. YASS!
%%mfa_spec({calc_server, loop, [_]}) ->
%%  {ok,
%%    {chs,
%%      {env, [{str, "+"}]},
%%      {act,
%%        {env, [{str, "_@A/{trace, _ ,spawned, _,{_,_,[Secret]}}"}, {var, '_@A'}, {pat, {trace, undefined, spawned, undefined, {undefined, undefined, [undefined]}}}]},
%%        fun({trace, _, spawned, _, {_, _, [Secret]}}) -> true; (_) -> false end,
%%        fun({trace, _, spawned, _, {_, _, [Secret]}}) ->
%%          {rec,
%%            {env, [{str, "rec X"}, {var, 'X'}]},
%%            fun X() ->
%%              {'and',
%%                {env, [{str, "and"}]},
%%                {chs,
%%                  {env, [{str, "+"}]},
%%                  {act,
%%                    {env, [{str, "_@B/{trace, _, send, Msg, _} when Secret=:=Msg"}, {var, '_@B'}, {pat, {trace, undefined, send, undefined, undefined}}]},
%%                    fun({trace, _, send, Msg, _}) when Secret =:= Msg -> true; (_) -> false end,
%%                    fun(_) ->
%%                      {no, {env, [{str, "no"}]}}
%%                    end
%%                  },
%%                  {act,
%%                    {env, [{str, "not(_@B/{trace, _, send, Msg, _} when Secret=:=Msg)"}, {var, '_@B'}, {pat, {trace, undefined, send, undefined, undefined}}]},
%%                    fun({trace, _, send, Msg, _}) when Secret =:= Msg -> false; (_) -> true end,
%%                    fun(_) -> % TODO: This must be always underscore to admit those messages (and not fail on a pattern match) that may be different that the trace pattern we expect ({trace, ..} in this case).
%%                      {yes, {env, [{str, "yes"}]}}
%%                    end
%%                  }
%%                },
%%                {chs,
%%                  {env, [{str, "+"}]},
%%                  {act,
%%                    {env, [{str, "_@B/{trace, _, send, Msg, _} when Secret=/=Msg"}, {var, '_@B'}, {pat, {trace, undefined, send, undefined, undefined}}]},
%%                    fun({trace, _, send, Msg, _}) when Secret =/= Msg -> true; (_) -> false end,
%%                    fun(_) ->
%%                      {var, {env, [{str, "X"}, {var, 'X'}]}, X}
%%                    end
%%                  },
%%                  {act,
%%                    {env, [{str, "not(_@B/{trace, _, send, Msg, _} when Secret=/=Msg)"}, {var, '_@B'}, {pat, {trace, undefined, send, undefined, undefined}}]},
%%                    fun({trace, _, send, Msg, _}) when Secret =/= Msg -> false; (_) -> true end,
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
%%        {env, [{str, "not(_@A/{trace, _ ,spawned, _,{_,_,[Secret]}})"}, {var, '_@A'}, {pat, {trace, undefined, spawned, undefined, {undefined, undefined, [undefined]}}}]},
%%        fun({trace, _, spawned, _, {_, _, [Secret]}}) -> false; (_) -> true end,
%%        fun({trace, _, spawned, _, {_, _, [Secret]}}) ->
%%          {yes, {env, [{str, "yes"}]}}
%%        end
%%      }
%%    }
%%  };
%%mfa_spec(_) ->
%%  undefined.


% This could be a security property, where the secret that we started with cannot
% be sent to the clients. YASS!
mfa_spec({calc_server, loop, [_]}) ->
  {ok,
    {chs,
      {env, [{str, "+"}]},
      {act,
        {env, [{str, "_@A/{trace, _ ,spawned, _,{_,_,[Secret]}}"}, {var, '_@A'}, {pat, {trace, undefined, spawned, undefined, {undefined, undefined, [undefined]}}}]},
        fun({trace, _, spawned, _, {_, _, [Secret]}}) -> true; (_) -> false end,
        fun({trace, _, spawned, _, {_, _, [Secret]}}) ->
          {rec,
            {env, [{str, "rec X"}, {var, 'X'}]},
            fun X() ->
              {'and',
                {env, [{str, "and"}]},
                {chs,
                  {env, [{str, "+"}]},
                  {act,
                    {env, [{str, "_@B/{trace, _, send, Msg, _} when Secret=:=Msg"}, {var, '_@B'}, {pat, {trace, undefined, send, undefined, undefined}}]},
                    fun({trace, _, send, Msg, _}) when Secret =:= Msg ->
                      true;
                      (_) -> false
                    end,
                    fun({trace, _, send, Msg, _}) ->
                      {no, {env, [{str, "no"}]}}
                    end
                  },
                  {act,
                    {env, [{str, "!!!!not(_@B/{trace, _, send, Msg, _} when Secret=:=Msg)"}, {var, '_@B'}, {pat, {trace, undefined, send, undefined, undefined}}]},
                    fun(E = {trace, _, send, Msg, _}) when Secret =:= Msg ->
                      ?INFO("~n~n------ RIGHT CHOICE and event is ~p~n", [E]),
                      false;
                      (E) ->
                        ?INFO("~n~n------ RIGHT CHOICE and event is ~p~n", [E]),
                        true
                    end, % TODO: This should FAIL!
                    fun(_) -> % TODO: This must be always underscore to admit those messages (and not fail on a pattern match) that may be different that the trace pattern we expect ({trace, ..} in this case).
                      {yes, {env, [{str, "yes"}]}}
                    end
                  }
                },
                {chs,
                  {env, [{str, "+"}]},
                  {act,
                    {env, [{str, "_@B/_"}, {var, '_@B'}, {pat, undefined}]},
                    fun(_) -> true; (_) -> false end, % TODO: This was made _ to match any other event. But it's to a thing done automatically done by the synthesis, but the user when specifying the formula!
                    fun(_) ->
                      {var, {env, [{str, "X"}, {var, 'X'}]}, X}
                    end
                  },
                  {act,
                    {env, [{str, "not(_@B/_)"}, {var, '_@B'}, {pat, undefined}]},
                    fun(_) -> false; (_) -> true end, % TODO: This must be always underscore to admit those messages (and not fail on a pattern match) that may be different that the trace pattern we expect ({trace, ..} in this case).
                    fun(_) -> % TODO: This must be always underscore to admit those messages (and not fail on a pattern match) that may be different that the trace pattern we expect ({trace, ..} in this case).
                      {yes, {env, [{str, "yes"}]}}
                    end
                  }
                }
              }
            end
          }
        end
      },
      {act,
        {env, [{str, "not(_@A/{trace, _ ,spawned, _,{_,_,[Secret]}})"}, {var, '_@A'}, {pat, {trace, undefined, spawned, undefined, {undefined, undefined, [undefined]}}}]},
        fun({trace, _, spawned, _, {_, _, [Secret]}}) -> false; (_) -> true end, % TODO: This is the correct way.
        fun(_) -> % TODO: This must be always underscore to admit those messages (and not fail on a pattern match) that may be different that the trace pattern we expect ({trace, ..} in this case).
          {yes, {env, [{str, "yes"}]}}
        end
      }
    }
  };
mfa_spec(_) ->
  undefined.