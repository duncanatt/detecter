%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(file_server_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->

  Dispatch = cowboy_router:compile([
    {'_', Routes = [
      {"/[...]", cowboy_static, {priv_dir, coordination_2022, "", [
        {mimetypes, cow_mimetypes, all},
        {dir_handler, directory_h}
      ]}}
    ]}
  ]),
%%  [{"/", hello_handler, []}]}

  % Set the Ranch transport options for the TCP layer.
  TransOpts = #{num_acceptors => 10, socket_opts => [{port, 8080}]},

  % Set the Ranch protocol options for the HTTP layer.
  ProtoOpts = #{
    env => #{dispatch => Dispatch},
    middlewares => [cowboy_router, directory_lister, cowboy_handler]
  },

  % Start Cowboy over plain HTTP.
  {ok, _} = cowboy:start_clear(http, TransOpts, ProtoOpts),
  io:format("Running Cowboy with Ranch options: ~p.~n", [TransOpts]),
  io:format("Configured routes: ~p.~n", [Routes]),

  file_server_sup:start_link().


stop(_State) ->
  ok = cowboy:stop_listener(http).
