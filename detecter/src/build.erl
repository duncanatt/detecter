%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Provides common and experimental build functions for custom EDoc.
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
-module(build).
-author("Duncan Paul Attard").

%%% Public API.
-export([edoc/0, mkdoc/0, leex/0, yecc/0]).

%%% Types.
-export_type([]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%%% Directory of compiled EDoc output.
-define(DOC_DIR, "doc").

%%% Name of custom CSS file used by EDoc.
-define(CSS_FILE, "priv/edoc/edoc.css").

%%% Application name.
-define(APP_NAME, "detectEr").

%%% Custom EDoc macros to facilitate EDoc writing and visual parsing of
%%% documentation in source code. Might be modified in future to generate XML
%%% rather than HTML directly.
-define(MACROS, [
  {sect, "<h4 id=\"#{@?}\">{@?}</h4>"},
%%  {sectl, "<a href=\"#{@?}\">{@?}</a>"},
  {returns, "<p><b>Returns:</b> {@?}</p>"},
  {params, "<dl>{@?}</dl>"},
  {name, "<dt>`{@?}'</dt>"},
  {desc, "<dd>{@?}</dd>"},
  {code, "<code>{@?}</code>"},
  {emph, "<b>{@?}</b>"},
  {mono, "<tt>{@?}</tt>"},
  {par, "<p>{@?}</p>"},
  {dl, "<dl>{@?}</dl>"},
  {ol, "<ol>{@?}</ol>"},
  {ul, "<ul>{@?}</ul>"},
  {item, "<li>{@?}</li>"},
  {term, "<dt>{@?}</dt>"},
  {eg, "<i>e.g.,</i>"},
  {ie, "<i>i.e.,</i>"},
  {etc, "<i>etc.</i>"},
  {ge, "&gt;="},
  {le, "&lt;="},
  {gt, "&gt;"},
  {lt, "&lt;"}
]).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-spec edoc() -> any().
edoc() ->
  Params = [
    {title, ?APP_NAME},
    {dir, ?DOC_DIR},
    {stylesheet_file, ?CSS_FILE},
%%    {private, true},
    {def, ?MACROS}
%%    {todo, true}
%%    {doclet, edoc_doclet_mkdocs}
  ],
  edoc:application(detecter, ".", Params).

-spec mkdoc() -> any().
mkdoc() ->
  Params = [
    {title, ?APP_NAME},
%%    {dir, ?DOC_DIR},
%%    {stylesheet_file, ?CSS_FILE},
%%    {private, true},
    {def, ?MACROS},
%%    {todo, true},
    {doclet, edoc_doclet_mkdocs}
  ],
  edoc:application(detecter, ".", Params).


%%leex() ->
%%  leex:file("priv/log_lexer.xrl", {scannerfile, "src/tracing/log_lexer.erl"}).

-spec leex() -> any().
leex() ->
%%  leex:file("priv/hml_lexer.xrl", {scannerfile, "src/monitoring/hml_lexer.erl"}),
%%  leex:file("priv/log_lexer.xrl", {scannerfile, "src/tracing/log_lexer.erl"}),
%%  leex:file("priv/shml_lexer.xrl", {scannerfile, "src/monitoring/shml_lexer.erl"}),
  leex:file("priv/maxhml_lexer.xrl", {scannerfile, "src/synthesis/maxhml_lexer.erl"}).

%%yecc() ->
%%  yecc:file("priv/log_parser.yrl", {parserfile, "src/tracing/log_parser.erl"}).

-spec yecc() -> any().
yecc() ->
%%  yecc:file("priv/hml_parser.yrl", {parserfile, "src/monitoring/hml_parser.erl"}),
%%  yecc:file("priv/log_parser.yrl", {parserfile, "src/tracing/log_parser.erl"}).
%%  yecc:file("priv/shml_parser.yrl", [{parserfile, "src/monitoring/shml_parser.erl"}, {verbose, true}]),
  yecc:file("priv/maxhml_parser.yrl", [{parserfile, "src/synthesis/maxhml_parser.erl"}, {verbose, true}]).

