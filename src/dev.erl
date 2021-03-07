%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Module description (becomes module heading).
%%%
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
-module(dev).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([profile/2]).

%%% Callbacks/Internal.
-export([]).

%%% Types.
-export_type([]).

%%% Implemented behaviors.
%-behavior().


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Records the time taken for the specified function to execute to
%%      completion.
%%
%% {@params
%%   {@name Fun}
%%   {@desc Function to profile.}
%%   {@name Tag}
%%   {@desc Optional tag, such as a friendly function name, that identifies the
%%          function.
%%   }
%% }
%%
%% {@par Time is measured in wall clock time and shown in milliseconds.}
%%
%% {@returns Return value of `Fun'.}
-spec profile(Fun :: function(), Tag :: string()) -> term().
profile(Fun, Tag) when is_function(Fun) ->
  {Us, Ret} = timer:tc(Fun),
  {name, Name} = erlang:fun_info(Fun, name),
  io:format("~n[~s, ~s]: ~bus (= ~.4fms)~n~n", [Name, Tag, Us, Us / 1000]),
  Ret.


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------
