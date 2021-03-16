%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Generic file poller behaviour.
%%%
%%%
%%% {@sect Callbacks:}
%%% {@dl
%%%   {@item
%%%     <tt>
%%%       init(Args :: {@link list()}) ->
%%%       @{ok, State :: {@link term()}@} | @{stop, Reason :: {@link term()}@}
%%%     </tt>
%%%   }
%%%   {@desc Called by the generic file poller upon initialization, but prior to
%%%          starting the main polling loop. The callback is expected to return
%%%          the initialization state `@{ok, State@}' specific to the file
%%%          poller implementation, or a termination reason `@{stop, Reason@}'.
%%%   }
%%%   {@item
%%%     <tt>
%%%       handle_line(Line :: [{@link byte()}],
%%%       LineInfo :: {@link line_info()}, State :: {@link term()}) ->
%%%       @{ok, NewState :: {@link term()}@}
%%%     </tt>
%%%   }
%%%   {@desc Called by the generic file poller whenever a new line is parsed to
%%%          completion. The `Line' consists of a list of bytes that do not
%%%          include the new line delimiter characters. Lines are delimited by
%%%          `\n' in Unix, Linux and macOS, and `\r\n' in Windows. `LineInfo'
%%%          contains the information specific to the parsed line (see
%%%          {@link line_info()}), while State is the opaque state specific to
%%%          the file poller implementation, passed to the generic file poller
%%%          in the initialization phase through {@link init/1}. The callback is
%%%          expected to return the updated state `@{ok, NewState@}'.
%%%   }
%%%   {@item
%%%     <tt>
%%%       terminate(Reason :: {@link term()}, State :: {@link term()}) ->
%%%       {@link any()}
%%%     </tt>
%%%   }
%%%   {@desc Called by the generic file poller prior to termination. `State' is
%%%          the last updated state specific to the file poller implementation.
%%%          The callback may be used by the implementation to perform cleanup.
%%%          Optional.
%%%   }
%%% }
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
-module(gen_file_poller).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([start/3, start/4, start_link/3, start_link/4, stop/1]).
-export([new_line_info/2, line_info_byte_off/1, line_info_line_num/1]).

%%% Callbacks.
-export([init_it/6]). %% Gen module.
-export([system_continue/3, system_terminate/4, system_get_state/1,
  system_replace_state/2, system_code_change/4]). %% Sys module.

%%% Types.
-export_type([line_info/0, option/0, options/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Zero hour from when poller begins processing lines. TODO: Check.
-define(ZERO_HOUR, {{1970, 1, 1}, {0, 0, 0}}).

%% Default polling interval in ms.
-define(POLL_MS, 500).

%% Default buffer length in bytes.
-define(BUF_LEN, 3).

%% Start byte in source file.
-define(START_BYTE, 0).

%% Start line in source file.
-define(START_LINE, 1).

%% Used by the generic file poller to keep track of the buffer status while it
%% incrementally processes segments of the buffer to parse individual lines.
%%
%% {@dl
%%   {@item `byte_cnt'}
%%   {@desc Number of bytes processed from the buffer.}
%%   {@item `line_cnt'}
%%   {@desc Number of new lines processed when reading the buffer.}
%%   {@item `buf_left'}
%%   {@desc Bytes left unprocessed in the buffer.}
%% }
-record(buf_stat, {
  byte_cnt = 0 :: offset(),
  line_cnt = 0 :: non_neg_integer(),
  buf_left = [] :: [byte()]
}).

%% Used by the generic file poller to provide line information to the caller of
%% the `handle_line/3' callback.
%% {@dl
%%   {@item `byte_off'}
%%   {@desc Starting byte offset of the current line in source file.}
%%   {@item `line_num'}
%%   {@desc Current line number in source file.}
%% }
-record(line_info, {
  byte_off = 0 :: offset(),
  line_num = 1 :: pos_integer()
}).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type offset() :: non_neg_integer().
%% Byte offset in the source file.

-type buf_stat() :: #buf_stat{}.
%% Buffer information.

%%-opaque line_info() :: #line_info{}.
-type line_info() :: #line_info{}.
%% Line location information.

%% Process registered name.
-type emgr_name() :: {local, atom()} | {global, term()} | {via, Mod :: module(), Name :: term()}.

%% Poll interval in ms option.
-type option() :: {poll_ms, timeout()}.

%% Generic file poller option configuration list.
-type options() :: [option()].


%%% ----------------------------------------------------------------------------
%%% Callback definitions.
%%% ----------------------------------------------------------------------------

%% Called by the generic file poller upon initialization, but prior to starting
%% the main polling loop.
-callback init(Args :: list()) ->
  {ok, State :: term()} | {stop, Reason :: term()}.

%% Called by the generic file poller whenever a new line is parsed to
%% completion.
-callback handle_line(Line :: [byte()], LineInfo :: line_info(), State :: term()) ->
  {ok, NewState :: term()}.

%% Called by the generic file poller prior to termination. Optional.
-callback terminate(Reason :: term(), State :: term()) -> any().

-optional_callbacks([terminate/2]).


%%% ----------------------------------------------------------------------------
%%% Public data API.
%%% ----------------------------------------------------------------------------

%% @doc Returns a new line information record from the specified arguments.
%%
%% {@params
%%   {@name ByteOff}
%%   {@desc Starting byte offset of the current line in source file.}
%%   {@name LineNum}
%%   {@desc Current line number in source file.}
%% }
%%
%% {@returns A new line information record.}
-spec new_line_info(ByteOff, LineNum) -> line_info()
  when
  ByteOff :: offset(),
  LineNum :: pos_integer().
new_line_info(ByteOff, LineNum) ->
  #line_info{byte_off = ByteOff, line_num = LineNum}.

%% @doc Returns the byte offset from the specified line information record.
%%
%% {@params
%%   {@name LineInfo}
%%   {@desc Line information record.}
%% }
%%
%% {@returns The byte offset.}
-spec line_info_byte_off(LineInfo :: line_info()) -> offset().
line_info_byte_off(#line_info{byte_off = ByteOff}) ->
  ByteOff.

%% @doc Returns the line number from the specified line information record.
%%
%% {@params
%%   {@name LineInfo}
%%   {@desc Line information record.}
%% }
%%
%% {@returns The line number.}
-spec line_info_line_num(LineInfo :: line_info()) -> pos_integer().
line_info_line_num(#line_info{line_num = LineNum}) ->
  LineNum.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Starts a standalone file poller process that is not part of a
%% supervision tree.
%%
%% {@par For description of arguments and return values see
%%       {@link start_link/4}.
%% }
-spec start(Mod, File, Opts) -> {ok, Pid :: pid()}
  when
  Mod :: module(),
  File :: string(),
  Opts :: options().
start(Mod, File, Opts) when
  is_atom(Mod), is_list(File), is_list(Opts) ->
  gen:start(?MODULE, nolink, Mod, [File], Opts).

%% @doc Starts a named standalone file poller process that is not part of a
%% supervision tree.
%%
%% {@par For description of arguments and return values see
%%       {@link start_link/4}.
%% }
-spec start(Name, Mod, File, Opts) -> {ok, Pid :: pid()}
  when
  Name :: emgr_name(),
  Mod :: module(),
  File :: file:filename(),
  Opts :: options().
start(Name, Mod, File, Opts) when
  is_atom(Mod), is_list(File), is_list(Opts) ->
  gen:start(?MODULE, nolink, Name, Mod, [File], Opts).

%% @doc Starts a file poller process as part of a supervision tree.
%%
%% {@par For description of arguments and return values see
%%       {@link start_link/4}.
%% }
-spec start_link(Mod, File, Opts) -> {ok, Pid :: pid()}
  when
  Mod :: module(),
  File :: file:filename(),
  Opts :: options().
start_link(Mod, File, Opts) when
  is_atom(Mod), is_list(File), is_list(Opts) ->
  gen:start(?MODULE, link, Mod, [File], Opts).

%% @doc Starts a file poller process as part of a supervision tree.
%%
%% {@params
%%   {@name Name}
%%   {@desc File poller process registered name.}
%%   {@name Mod}
%%   {@desc Module implementing the callback `Mod:handle_line/3'.}
%%   {@name File}
%%   {@desc Source file to monitor for changes.}
%%   {@name Opts}
%%   {@desc File poller configuration options. Only one option is supported:
%%          {@dl
%%            {@term `@{poll_ms, Ms@}'}
%%            {@desc Polling interval in milliseconds. Defaults to 500 ms.}
%%          }
%%   }
%% }
%%
%% {@returns `@{ok, Pid@}', where `Pid' is the PID of the file poller process.}
-spec start_link(Name, Mod, File, Opts) -> {ok, Pid :: pid()}
  when
  Name :: emgr_name(),
  Mod :: module(),
  File :: file:filename(),
  Opts :: options().
start_link(Name, Mod, File, Opts) when
  is_atom(Mod), is_list(File), is_list(Opts) ->
  gen:start(?MODULE, link, Name, Mod, [File], Opts).

%% @doc Shuts down a file poller process.
%%
%% {@params
%%   {@name Name}
%%   {@desc File poller PID or registered name.}
%% }
%%
%% {@returns Does not return in case of errors, otherwise `ok' to indicate a
%%           successful shut down.
%% }
-spec stop(Name :: pid() | atom()) -> ok | no_return().
stop(Name) ->
  gen:stop(Name).


%%% ----------------------------------------------------------------------------
%%% gen module callbacks.
%%% ----------------------------------------------------------------------------

%% @private Initializes and launches main file poller loop.
%%
%% {@params
%%   {@name Starter}
%%   {@desc PID of the process starting the file poller.}
%%   {@name Parent}
%%   {@desc PID of the process the file poller is linked to or `self', if the
%%          file poller is standalone. TODO: Confirm this.
%%   }
%%   {@name Name}
%%   {@desc File poller PID or registered name.}
%%   {@name Mod}
%%   {@desc Module implementing the callbacks.}
%%   {@name Args}
%%   {@desc File poller initialization arguments. The only argument currently
%%          possible is the filename where the source file to be processed
%%          resides.
%%   }
%%   {@name Opts}
%%   {@desc File poller configuration options.}
%% }
%%
%% {@returns Does not return.}
-spec init_it(Starter, Parent, Name, Mod, Args, Opts) -> no_return()
  when
  Starter :: pid(),
  Parent :: pid() | self,
  Name :: pid() | emgr_name(),
  Mod :: module(),
  Args :: list(),
  Opts :: options().
init_it(Starter, Parent, Name, Mod, Args, Opts) ->
  io:format("init_it: Starter=~p, Parent=~p, Name=~p, Mod=~p, Args=~p, Opts=~p~n",
    [Starter, Parent, Name, Mod, Args, Opts]),

  % Get server name and debug options.
  Name0 = gen:name(Name),
  Debug = gen:debug_options(Name0, Opts),

  % Get optional config properties.
  PollMs = proplists:get_value(poll_ms, Opts, ?POLL_MS),

  % Invoke 'init' callback to obtain poller start state.
  State = init(Starter, Parent, Name, Mod, Args),
  loop(Parent, Name0, State, Mod, hd(Args), PollMs, ?ZERO_HOUR, ?START_BYTE, ?START_LINE, Debug).


%%% ----------------------------------------------------------------------------
%%% sys module callbacks.
%%% ----------------------------------------------------------------------------

%% @private Called indirectly via sys:handle_system_msg/6 when sys:resume/{1,2},
%%  % sys:trace/2, etc. is invoked on the poller.
-spec system_continue(Parent, Debug, Misc) -> no_return()
  when
  Parent :: pid(),
  Debug :: [sys:dbg_opt()],
  Misc :: term().
system_continue(Parent, Debug,
    _Misc = [Name, State, Mod, File, PollMs, LastModTime, ByteOffset, LineOffset]) ->
  loop(Parent, Name, State, Mod, File, PollMs, LastModTime, ByteOffset, LineOffset, Debug).

%% @private Called indirectly via sys:handle_system_msg/6 when
%% sys:terminate/{2,3} is invoked on the poller.
-spec system_terminate(Reason, Parent, Debug, Misc) -> no_return()
  when
  Reason :: term(),
  Parent :: pid(),
  Debug :: [sys:dbg_opt()],
  Misc :: term().
system_terminate(Reason, _Parent, Debug,
    [Name, State, Mod, _File, _PollMs, _LastModTime, _ByteOffset, _LineOffset]) ->

  % Invoke 'terminate' callback if it exists, otherwise do nothing.
  case terminate(Reason, Name, State, Mod, Debug) of
    {'EXIT', Type, Reason0, Stacktrace} ->
      % Either exit or error occurred in 'terminate' callback. Raise Erlang
      % error.
      erlang:raise(Type, Reason0, Stacktrace);
    {ok, _} ->
      % Return value from terminate 'callback' is always discarded; so is any
      % value thrown by said callback. Poller exists with reason from system
      % terminate. Note that since proc_lib is used underneath, the termination
      % reasons `shutdown' and `{shutdown, _}' (in addition to the standard one
      % used by Erlang, `normal') enable a clean exit without any errors (as
      % used in OTP).
      exit(Reason)
  end.

%% @private Called indirectly via sys:handle_system_msg/6 when
%% sys:get_state/{1,2} is invoked on the poller.
-spec system_get_state(Misc :: list()) -> {ok, State :: term()}.
system_get_state([_Name, State, _Mod, _File, _PollMs, _LastModTime, _ByteOffset, _LineOffset]) ->
  % Return poller state.
  {ok, State}.

%% @private Called indirectly via sys:handle_system_msg/6 when
%% sys:replace_state/{2,3} is invoked on the poller.
-spec system_replace_state(StateFun :: function(), Misc :: list()) ->
  {ok, NewState :: term(), NewMisc :: list()}.
system_replace_state(StateFun,
    [Name, State, Mod, File, PollMs, LastModTime, ByteOffset, LineOffset]) ->
  % Replace poller state. State returned by 'StateFun' must match exactly the
  % one returned by system_get_state. For instance:
  % sys:replace_state(gen_file_poller_impl, fun({lines_read, Cnt}) -> {lines_read, 0} end).
  NewState = StateFun(State),
  {ok, NewState,
    [Name, NewState, Mod, File, PollMs, LastModTime, ByteOffset, LineOffset]}.

%% @private Required callback by sys module, but which is not implemented yet.
-spec system_code_change(Misc, GenMod, OldVsn, Extra) -> no_return()
  when
  Misc :: list(),
  GenMod :: module(),
  OldVsn :: term(),
  Extra :: term().
system_code_change([_Name, _State, _Mod, _File, _PollMs, _LastModTime, _ByteOffset, _LineOffset],
    _GenMod, _OldVsn, _Extra) ->
  error(not_implemented).

%% @private Prints debug information to standard IO.
-spec write_debug(IoDev, Event, Name) -> ok
  when
  IoDev :: file:io_device(),
  Event :: term(),
  Name :: atom().
write_debug(IoDev, Event, Name) ->
  io:format(IoDev, "*DBG* ~p event = ~p~n", [Name, Event]).


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% @private Monitors the specified file for changes and parses lines when new
%% data is detected.
%%
%% {@par The file is monitored for changes periodically by polling. The polling
%%       frequency is adjustable via the `@{poll_ms, PollMs@}' option.
%% }
%% {@par `File' is parsed in read-only mode whenever a change in its last
%%       modified timestamp attribute is detected. The callback
%%       `Mod:handle_line/3' is invoked once for each successfully parsed line,
%%       with the line information provided accordingly.
%% }
%%
%% {@params
%%   {@name Parent}
%%   {@desc PID of the process the file poller is linked to or `self', if the
%%          file poller is standalone.}
%%   {@name Name}
%%   {@desc File poller PID or registered name.}
%%   {@name State}
%%   {@desc File poller state.}
%%   {@name Mod}
%%   {@desc Module implementing the callbacks.}
%%   {@name File}
%%   {@desc Source file to monitor for changes.}
%%   {@name PollMs}
%%   {@desc File polling frequency.}
%%   {@name LastModTime}
%%   {@desc Last known file modification time.}
%%   {@name ByteOff}
%%   {@desc (0-based) byte offset from where to start reading the source file.}
%%   {@name LineNum}
%%   {@desc (1-based) line number that tracks the new lines processed in the
%%          source file byte stream up to the current point.
%%   }
%%   {@name Debug}
%%   {@desc Debugging information and flags.}
%% }
%%
%% {@returns Does not return.}
-spec loop(Parent, Name, State, Mod, File, PollMs, LastModTime, ByteOff,
    LineNum, Debug) -> no_return()
  when
  Parent :: pid() | self,
  Name :: pid() | emgr_name(),
  State :: term(),
  Mod :: module(),
  File :: file:filename(),
  PollMs :: timeout(),
  LastModTime :: file:date_time(),
  ByteOff :: offset(),
  LineNum :: pos_integer(),
  Debug :: [sys:dbg_opt()].
loop(Parent, Name, State, Mod, File, PollMs, LastModTime, ByteOff, LineNum, Debug) ->
  case filelib:last_modified(File) of
    ModTime when is_tuple(ModTime), ModTime > LastModTime ->

      ?DEBUG("File '~s' has been modified, parsing available lines.", [File]),

      % File has been modified since last check. Open for reading and process
      % all available lines, starting from the last known byte offset,
      % continuing until EOF is met.
      {ok, IoDev} = file:open(File, [read]),
      {ok, NextByteOffset, NextLineOffset, NewState} =
        parse_lines(Name, State, Mod, IoDev, ByteOff, LineNum),

      % Close file to relinquish OS file handle.
      file:close(IoDev),

      % Log debug information used by system module.
      SystemEvent = {bytes_read, ByteOff, NextByteOffset, NextByteOffset - ByteOff},
      Debug0 = sys:handle_debug(Debug, fun write_debug/3, Name, SystemEvent),

      % Parsing the file advances the last known byte and line offset forwards.
      % Handle system messages and loop with updated values.
      handle_system_msg(Parent, Name, NewState, Mod, File, PollMs, ModTime, NextByteOffset, NextLineOffset, Debug0),
      loop(Parent, Name, NewState, Mod, File, PollMs, ModTime, NextByteOffset, NextLineOffset, Debug0);

    _ ->

      % No changes detected in file. Handle system messages.
      handle_system_msg(Parent, Name, State, Mod, File, PollMs, LastModTime, ByteOff, LineNum, Debug),
      loop(Parent, Name, State, Mod, File, PollMs, LastModTime, ByteOff, LineNum, Debug)
  end.

%% @private Parses the lines by reading from the specified IO device, starting
%% at the set byte offset and continuing until `<eof>' is met.
%%
%% {@par Lines are delimited by `\n' in Unix, Linux and macOS, and `\r\n' in
%%       Windows. The callback `Mod:handle_line/3' is invoked once for each
%%       successfully parsed line, with the line information provided
%%       accordingly.
%% }
%% {@par No attempt is made to handle errors that may arise as a result of the
%%       processing taking place inside the callback `Mod:handle_line/3'.
%% }
%%
%% {@params
%%   {@name Name}
%%   {@desc File poller PID or registered name.}
%%   {@name State}
%%   {@desc File poller state.}
%%   {@name Mod}
%%   {@desc Module implementing the callbacks.}
%%   {@name IoDev}
%%   {@desc Device identifier of the open file being processed.}
%%   {@name ByteOff}
%%   {@desc (0-based) byte offset from where to start reading the source file.}
%%   {@name LineNum}
%%   {@desc (1-based) line number that tracks the new lines processed in the
%%          source file byte stream up to the current point.
%%   }
%% }
%%
%% {@returns The status of the processing operation:
%%           {@dl
%%             {@item `@{ok, NextByteOff, NextLineNum, State@}'}
%%             {@desc Next byte offset `NextByteOff' in `IoDev' from where to
%%                    resume processing the source file upon next call, together
%%                    with the last line number parsed from the byte stream.
%%             }
%%           }
%% }
-spec parse_lines(Name, State, Mod, IoDev, ByteOff, LineNum) ->
  {ok, NextByteOff :: offset(), NextLineNum :: pos_integer(), State :: term()}
  when
  Name :: pid() | emgr_name(),
  State :: term(),
  Mod :: module(),
  IoDev :: file:io_device(),
  ByteOff :: offset(),
  LineNum :: pos_integer().
parse_lines(Name, State, Mod, IoDev, ByteOff, LineNum) ->
  parse_lines(Name, State, Mod, IoDev, [], ByteOff, LineNum).

%% @private See {@link parse_lines/6}.
-spec parse_lines(Name, State, Mod, IoDev, Buf, ByteOff, LineNum) ->
  {ok, NextByteOff :: offset(), NextLineNum :: offset(), State :: term()}
  when
  Name :: pid() | emgr_name(),
  State :: term(),
  Mod :: module(),
  IoDev :: file:io_device(),
  Buf :: [byte()],
  ByteOff :: offset(),
  LineNum :: pos_integer().
parse_lines(Name, State, Mod, IoDev, Buf, ByteOff, LineNum) ->
  case parse_line(Buf) of
    {ok,
      #buf_stat{byte_cnt = ByteCnt, line_cnt = LineCnt, buf_left = BufLeft},
      Line} ->

      % A maximum of one line is parsed for the current implementation of
      % gen_file_poller.
      ?assertEqual(1, LineCnt),

      % Parse completed successfully using bytes from current buffer. Invoke
      % callback, passing in the bytes read, byte offset and line offset info.
      NewState = handle_line(Name, State, Mod, Line,
        new_line_info(ByteOff, LineNum)),

      ?TRACE("Buffer left ~p, parsed line ~p (read ~p bytes, ~p lines).",
        [BufLeft, Line, ByteCnt, LineCnt]),

      % Parse next line from the bytes remaining in buffer.
      parse_lines(Name, NewState, Mod, IoDev, BufLeft, ByteOff + ByteCnt, LineNum + LineCnt);

    {fault,
      #buf_stat{byte_cnt = ByteCnt, line_cnt = LineCnt, buf_left = BufLeft}} ->

      % A minimum of one line is parsed for the current implementation of
      % gen_file_poller.
      ?assertEqual(0, LineCnt),

      % Parse could not be completed successfully due to a buffer fault. This
      % means that current buffer did not contain enough bytes to allow the line
      % to be parsed, and that the line contents go beyond the bytes contained
      % in the buffer. Load the next buffer of bytes from file, starting from
      % the last processed byte offset, shifted by the number of bytes read.
      NextByteOffset = ByteOff + ByteCnt,

      ?TRACE("Buffer left ~p, fault (at byte ~p).", [BufLeft, NextByteOffset]),

      case file:pread(IoDev, NextByteOffset, ?BUF_LEN) of
        % TODO: Use this to test!!!
%%      case file:pread(IoDev, NextByteOffset, random:uniform(10)) of
        {ok, NextBuf} ->

          % The unprocessed bytes from the previous buffer are prepended to the
          % newly-read buffer from file.
          NewBuf = BufLeft ++ NextBuf,

          ?TRACE("Next buffer ~p (from byte ~p to ~p).",
            [NextBuf, NextByteOffset, NextByteOffset + ?BUF_LEN]),

          % Parse next line from the bytes remaining in buffer from the last
          % incomplete parse AND the newly-read buffer. As the parse is
          % performed starting from the old buffer, the byte and line offsets
          % remain unchanged.
          parse_lines(Name, State, Mod, IoDev, NewBuf, ByteOff, LineNum);
        eof ->

          % EOF reached. No more bytes to parse.
          ?TRACE("EOF (at byte ~p, line ~p).", [NextByteOffset,
            LineNum + LineCnt]),

          {ok, ByteOff, LineNum, State}
      end
  end.

%% @private Parses the next line, starting from the beginning of the specified
%% buffer until the closest line delimiter is encountered.
%%
%% {@par Lines are delimited by `\n' in Unix, Linux and macOS, and `\r\n' in
%%       Windows.
%% }
%%
%% {@params
%%   {@name Buf}
%%   {@desc Buffer of bytes to process.}
%% }
%%
%% {@returns Status of the processing operation:
%%           {@dl
%%             {@item `@{ok, BufStat, Line@}'}
%%             {@desc Line parsing was completed and the resulting bytes are
%%                    returned in `Line'. The end of line byte(s) are not
%%                    included in `Line'.
%%             }
%%             {@item `@{fault, BufStat@}'}
%%             {@desc Line parsing was not completed due to the buffer not
%%                    containing sufficient bytes to enable the operation to
%%                    finish. The situation arises when the ending of the line
%%                    to be parsed goes beyond the byte content in the buffer,
%%                    meaning that additional buffered reads are required before
%%                    the line can be parsed ({@ie} until an end of line
%%                    delimiter is encountered in the buffer). A buffer `fault'
%%                    informs the caller that further parsing operations need to
%%                    be reissued with more bytes to the buffer before the line
%%                    can be parsed.
%%             }
%%           }
%%           {@par In both cases, `BufStat' contains the number of bytes read,
%%                 the number of lines encountered in the buffer stream ({@ie}
%%                 `0' if no line read, otherwise `1'), the bytes left
%%                  unprocessed in the buffer. Subsequent calls to parse the
%%                  line should always be issued with the unprocessed bytes in
%%                  the buffer, `BufStat#buf_left', prepended to the last read
%%                  buffer of bytes.
%%           }
%% }
-spec parse_line(Buf :: [byte()]) ->
  {fault, BufStat :: buf_stat()} |
  {ok, BufStat :: buf_stat(), Line :: [byte()]}.
parse_line(Buf) ->
  parse_line(Buf, 0, 0, []).

%% @private See {@link parse_line/1}.
-spec parse_line(Buf, BytesCnt, LinesCnt, Line) ->
  {fault, BufStat :: buf_stat()} |
  {ok, BufStat :: buf_stat(), Line :: [byte()]}
  when
  Buf :: [byte()],
  BytesCnt :: offset(),
  LinesCnt :: non_neg_integer(),
  Line :: [byte()].
parse_line([], BytesCnt, LinesCnt, Line) ->
  {fault, new_buf_stat(BytesCnt, LinesCnt, lists:reverse(Line))};
parse_line([13, 10 | Buf], BytesCnt, LinesCnt, Line) ->
  {ok, new_buf_stat(BytesCnt + 2, LinesCnt + 1, Buf), lists:reverse(Line)};
parse_line([10 | Buf], BytesCnt, LinesCnt, Line) ->
  {ok, new_buf_stat(BytesCnt + 1, LinesCnt + 1, Buf), lists:reverse(Line)};
parse_line([Byte | Buf], BytesCnt, LinesCnt, Line) ->
  parse_line(Buf, BytesCnt + 1, LinesCnt, [Byte | Line]).

%% @private Creates a new `buf_stat' record.
%%
%% {@params
%%   {@name ByteCnt}
%%   {@desc Number of bytes processed from the buffer.}
%%   {@name LineCnt}
%%   {@desc Number of new lines processed when reading the buffer.}
%%   {@name BufLeft}
%%   {@desc Bytes left unprocessed in the buffer.}
%% }
%%
%% {@returns A new `buf_stat' record.}
-spec new_buf_stat(ByteCnt, LineCnt, BufLeft) -> buf_stat()
  when
  ByteCnt :: offset(),
  LineCnt :: non_neg_integer(),
  BufLeft :: [byte()].
new_buf_stat(ByteCnt, LineCnt, BufLeft) ->
  #buf_stat{byte_cnt = ByteCnt,
    line_cnt = LineCnt,
    buf_left = BufLeft}.

%% @private Invokes the `Mod:init/1' callback to initialize the main file poller
%% state.
%%
%% {@params
%%   {@name Starter}
%%   {@desc PID of the process starting the file poller.}
%%   {@name Parent}
%%   {@desc PID of the process the file poller is linked to or `self', if the
%%          file poller is standalone.}
%%   {@name Name}
%%   {@desc File poller PID or registered name.}
%%   {@name Mod}
%%   {@desc Module implementing the callbacks.}
%%   {@name Args}
%%   {@desc File poller initialization arguments.}
%% }
%%
%% {@returns Does not return in case of errors, otherwise returns the
%%           initialization status:
%%           {@dl
%%             {@item `@{ok, State@}'}
%%             {@desc File poller initialized with the starting `State'.}
%%             {@item `@{stop, Reason@}'}
%%             {@desc File poller initialization halted with `Reason'.}
%%           }
%% }
-spec init(Starter, Parent, Name, Mod, Args) ->
  {ok, State :: term()} | {stop, Reason :: term()} | no_return()
  when
  Starter :: pid(),
  Parent :: pid() | self,
  Name :: pid() | emgr_name(),
  Mod :: module(),
  Args :: list().
init(Starter, _Parent, Name, Mod, Args) ->
  case try_callback(Mod, init, [Args]) of
    {ok, {ok, State}} ->
      % Valid state returned by init callback. Ack parent process (could be
      % supervisor or poller itself) and start main loop. Note that throws are
      % handled as valid return values: this is implemented in this manner to
      % keep the same behavior as gen_server.
      proc_lib:init_ack(Starter, {ok, self()}),
      State;
    {ok, {stop, Reason}} ->
      % Stop poller for specified reason.
      proc_lib:init_ack(Starter, {error, Reason}),
      exit(Reason);
    {ok, Else} ->
      % Unknown poller initial state. Exit with error.
      Error = {bad_return_value, Else},
      proc_lib:init_ack(Starter, {error, Error}),
      exit(Error);
    {'EXIT', Class, Reason, Stacktrace} ->
      % Either exit or error occurred. Raise Erlang error.
      gen:unregister_name(Name),
      proc_lib:init_ack(Starter,
        {error, terminate_reason(Class, Reason, Stacktrace)}
      ),
      erlang:raise(Class, Reason, Stacktrace)
  end.

%% @private Invokes the `Mod:handle_line/3' callback with the relevant line
%% information.
%%
%% {@params
%%   {@name Name}
%%   {@desc File poller PID or registered name.}
%%   {@name State}
%%   {@desc File poller state.}
%%   {@name Mod}
%%   {@desc Module implementing the callbacks.}
%%   {@name Line}
%%   {@desc Parsed line bytes.}
%%   {@name LineInfo}
%%   {@desc Line information record.}
%% }
%%
%% {@returns Does not return in case of errors, otherwise returns the new state:
%%           {@dl
%%             {@item `@{ok, NewState@}'}
%%             {@desc New file poller state.}
%%           }
%% }
-spec handle_line(Name, State, Mod, Line, LineInfo) ->
  {ok, NewState :: term()} | no_return()
  when
  Name :: pid() | emgr_name(),
  State :: term(),
  Mod :: module(),
  Line :: [byte()],
  LineInfo :: line_info().
handle_line(_Name, State, Mod, Line, LineInfo = #line_info{}) ->
  Args = [Line, LineInfo, State],
  case try_callback(Mod, handle_line, Args) of
    {ok, {ok, NewState}} ->
      % Valid new state returned by 'handle_line' callback.
      NewState;
    {ok, Else} ->
      % Unknown new state. Exit with error.
      Error = {bad_return_value, Else},
      exit(Error);
    {'EXIT', Class, Reason, Stacktrace} ->
      % Either exit or error occurred. Raise Erlang error.
      erlang:raise(Class, Reason, Stacktrace)
  end.

%% @private Invokes the `Mod:terminate/2' callback if implemented, otherwise
%% nothing is performed.
%%
%% {@par The return value from the callback is discarded.}
%%
%% {@params
%%   {@name Reason}
%%   {@desc Termination reason.}
%%   {@name Name}
%%   {@desc File poller PID or registered name.}
%%   {@name State}
%%   {@desc File poller terminating state.}
%%   {@name Mod}
%%   {@desc Module implementing the callbacks.}
%%   {@name Debug}
%%   {@desc Debugging information and flags.}
%% }
%%
%% {@returns Termination status:
%%           {@dl
%%             {@item `@{ok, ok@}'}
%%             {@name File poller default successful termination.}
%%             {@item `@{ok, Return@}'}
%%             {@name File poller successful termination after invoking
%%                    `Mod::terminate/2' callback.
%%             }
%%             {@item `@{'EXIT', Class, Reason, Stacktrace@}'}
%%             {@name Abnormal termination where `Class' is either `exit' or
%%                    `error'.
%%             }
%%           }
%% }
-spec terminate(Reason, Name, State, Mod, Debug) ->
  {ok, ok} | {ok, Return :: term()} |
  {'EXIT', Class :: exit | error, Reason :: term(), Stacktrace :: term()}
  when
  Reason :: term(),
  Name :: pid() | emgr_name(),
  State :: term(),
  Mod :: module(),
  Debug :: [sys:dbg_opt()].
terminate(Reason, _Name, State, Mod, _Debug) ->

  % Invoke terminate callback if exported, otherwise do nothing.
  case erlang:function_exported(Mod, terminate, 2) of
    false ->
      {ok, ok};
    true ->
      try_callback(Mod, terminate, [Reason, State])
  end.

%% @private Wraps around the callback invocation to perform exception handling
%% and translation.
%%
%% {@params
%%   {@name Mod}
%%   {@desc Module implementing the callbacks.}
%%   {@name Callback}
%%   {@desc Callback name.}
%%   {@name Args}
%%   {@desc Callback arguments.}
%% }
%%
%% {@returns Invocation status:
%%           {@dl
%%             {@item `@{ok, Return@}'}
%%             {@desc Successful callback invocation with result in `Return'.}
%%             {@item `@{'EXIT', Class, Reason, Stacktrace@}'}
%%             {@desc Unsuccessful callback invocation for the specified
%%                    `Reason'. The error may be classed as
%%                    `Class = exit | error'.
%%             }
%%           }
%% }
-spec try_callback(Mod, Callback, Args) -> {ok, Return :: term()} |
{'EXIT', Class :: exit | error, Reason :: term(), Stacktrace :: term()}
  when
  Mod :: module(),
  Callback :: atom(),
  Args :: list().
try_callback(Mod, Callback, Args) ->
  try
    {ok, apply(Mod, Callback, Args)}
  catch
    throw:Reason ->
      % Treat user-thrown exceptions as a legitimate and known exit reason.
      {ok, Reason};
    Class:Reason:Stacktrace ->
      % Treat exit or error exceptions as unexpected errors that should be
      % handled.
      {'EXIT', Class, Reason, Stacktrace}
  end.

%% @private Returns the termination reason based on the exception class
%% (also called the exception type).
%%
%% {@params
%%   {@name Class}
%%   {@desc Exception class denoting the kind of exception.}
%%   {@name Reason}
%%   {@desc Termination reason.}
%%   {@name Stacktrace}
%%   {@desc Detailed stacktrace causing the termination.}
%% }
%%
%% {@returns Termination reason.}
-spec terminate_reason(Class, Reason, Stacktrace) ->
  {Reason, Stacktrace} | Reason
  when
  Class :: error | exit,
  Reason :: term(),
  Stacktrace :: term().
terminate_reason(error, Reason, Stacktrace) -> {Reason, Stacktrace};
terminate_reason(exit, Reason, _Stacktrace) -> Reason.

%% @private Dequeues and handles system messages or times out if none exist.
%%
%% {@par System message handling enables the generic file poller to interact
%%       with calls issued by the `sys' module. System messages are handled by
%%       delegating to one of the following callbacks:
%%       {@ul
%%         {@item `Mod:system_continue/3'}
%%         {@item `Mod:system_terminate/4'}
%%         {@item `Mod:system_get_state/1'}
%%         {@item `Mod:system_replace_state/2'}
%%         {@item `Mod:system_code_change/4'}
%%       }
%% }
%%
%% {@returns `ok' or does not return.}
-spec handle_system_msg(Parent, Name, State, Mod, File, PollMs, LastModTime,
    ByteOff, LineNum, Debug) -> ok | no_return()
  when
  Parent :: pid() | self,
  Name :: pid() | emgr_name(),
  State :: term(),
  Mod :: module(),
  File :: string(),
  PollMs :: timeout(),
  LastModTime :: file:date_time(),
  ByteOff :: offset(),
  LineNum :: pos_integer(),
  Debug :: [sys:dbg_opt()].
handle_system_msg(Parent, Name, State, Mod, File, PollMs, LastModTime, ByteOff, LineNum, Debug) ->
  receive
    {system, From, Request} ->

      % Handle system messages.
      sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug,
        [Name, State, Mod, File, PollMs, LastModTime, ByteOff, LineNum])
  after PollMs ->
    ok
  end.