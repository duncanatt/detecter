%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Utility macros used for testing.
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
-author("Duncan Paul Attard").

%%% ----------------------------------------------------------------------------
%%% Client configuration macros.
%%% ----------------------------------------------------------------------------

%% Comment the lines below to disable testing and profiling modes. Testing mode
%% includes supporting code inside modules or module functions that is used for
%% testing particular code segments that should not be included in the release
%% version of the code. Likewise, profiling adds automated time keeping of
%% targeted functions to determine their efficiency. Both testing and profiling
%% modes are intended for development only, and should only be used to fine tune
%% the code and its operation.

%%-undef(TEST).
%%-define(TEST, true).
%%-define(PROFILE, true).


%%% ----------------------------------------------------------------------------
%%% Private macros (DO NOT MODIFY).
%%% ----------------------------------------------------------------------------

%% TODO: Comment the below, except profile.

%% TODO: Remove this commented bit..because probably is redundant.
%%-ifdef(TEST).
%%-define(TEST_TRACES_ETS_NAME, mon_state). % Monitor state ETS name.
%%-define(TESTER_REG_NAME, tester). % Tester registered process name.
%%-endif.

%% Inserts the specified expression only if the 'TEST' macro is defined. Used to
%% add extra information to the monitor which is useful when performing unit
%% tests, but would otherwise slow the monitor down when used in production.
-ifdef(TEST).
-define(exec_if_test(TestExpr, Default), TestExpr).
-else.
-define(exec_if_test(TestExpr, Default), Default).
-endif.

%% Inserts the specified expression only if the 'WEAVED' macro is defined.

%% Expands the macro expression only when the WEAVED macro is defined.
-ifdef(WEAVED).
-define(exec_if_weaved(WeavedExpr, Default), WeavedExpr).
-else.
-define(exec_if_weaved(WeavedExpr, Default), Default).
-endif.

%% Profile macro expanded to show the time taken for the code expression to
%% execute if the PROFILE macro is defined.
-ifdef(PROFILE).
-define(profile(Expr), dev:profile(fun() -> Expr end, ok)).
-define(profile(Expr, Tag), dev:profile(fun() -> Expr end, Tag)).
-else.
-define(profile(Expr), Expr).
-define(profile(Expr, _Tag), Expr).
-endif.




