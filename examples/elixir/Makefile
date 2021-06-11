################################################################################
## Project configurations                                                     ##
################################################################################

BIN=ebin
SRC=lib

define recursive
	$(shell find $(1) -name "*.$(2)")
endef

all: compile

clean:
	rm -f $(BIN)/*.beam $(BIN)/*.E $(BIN)/*.erl $(BIN)/*.tmp $(BIN)/*.app erl_crash.dump
	rm -df $(BIN)

compile: clean
	mkdir -p $(BIN)
	elixirc -pa $(BIN) -o $(BIN) $(call recursive,$(SRC),ex)

# How to run
#detecter/examples/elixir on  master [!?]
#❯ make
#rm -f ebin/*.beam ebin/*.E ebin/*.erl ebin/*.tmp ebin/*.app erl_crash.dump
#rm -df ebin
#mkdir -p ebin
#elixirc -pa ebin -o ebin 	lib/demo/calc_client.ex lib/demo/calc_server_bug.ex lib/demo/calc_server.ex
#Loading my Erlang shell..
#
#detecter/examples/elixir on  master [!?]
#❯ iex -pa ../../detecter/ebin -pa ebin
#Erlang/OTP 23 [erts-11.2.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe] [dtrace]
#
#Loading my Erlang shell..
#Interactive Elixir (1.11.4) - press Ctrl+C to exit (type h() ENTER for help)
#iex(1)> pid = Demo.CalcServerBug.start 0
##PID<0.108.0>
#iex(2)> send pid, {self, {:add, 10, 97}}
#{#PID<0.106.0>, {:add, 10, 97}}
#iex(3)> flush
#{:ok, -87}
#:ok
#iex(4)>

#iex(4)> :hml_eval.compile 'props/example_2.hml', [{:outdir, 'ebin'}, :v]
#:ok

#iex(10)> :monitor.start_online {Elixir.Demo.CalcServerBug, :start, [0]}, &:example_2.mfa_spec/1, []
#[INFO - <0.122.0> - tracer:234] - Started ROOT tracer <0.122.0> for top-level process <0.121.0>.
#*** [<0.122.0>] Instrumenting monitor for MFA pattern '{'Elixir.Demo.CalcServerBug',
#                                                                 loop,
#                                                                 [0]}'.
#{:ok, #PID<0.122.0>, #PID<0.123.0>}
#[INFO - <0.122.0> - tracer:754] - Instrumenting tracer <0.124.0> on MFA {'Elixir.Demo.CalcServerBug',loop,[0]} for process <0.123.0>.
#*** [<0.124.0>] Analyzing event {trace,<0.123.0>,spawned,<0.121.0>,
#                                      {'Elixir.Demo.CalcServerBug',loop,[0]}}.
#iex(11)> {:ok, _, pid} = :monitor.start_online {Elixir.Demo.CalcServerBug, :start, [0]}, &:example_2.mfa_spec/1, []
#[INFO - <0.127.0> - tracer:234] - Started ROOT tracer <0.127.0> for top-level process <0.126.0>.
#*** [<0.127.0>] Instrumenting monitor for MFA pattern '{'Elixir.Demo.CalcServerBug',
#                                                                 loop,
#                                                                 [0]}'.
#{:ok, #PID<0.127.0>, #PID<0.128.0>}
#[INFO - <0.127.0> - tracer:754] - Instrumenting tracer <0.129.0> on MFA {'Elixir.Demo.CalcServerBug',loop,[0]} for process <0.128.0>.
#*** [<0.129.0>] Analyzing event {trace,<0.128.0>,spawned,<0.126.0>,
#                                      {'Elixir.Demo.CalcServerBug',loop,[0]}}.
#iex(12)> pid
##PID<0.128.0>
#iex(13)> send pid, {self, {:add, 10, 97}}
#*** [<0.129.0>] Analyzing event {trace,<0.128.0>,'receive',
#                                      {<0.106.0>,{add,10,97}}}.
#{#PID<0.106.0>, {:add, 10, 97}}
#*** [<0.129.0>] Analyzing event {trace,<0.128.0>,send,{ok,-87},<0.106.0>}.
#*** [<0.129.0>] Reached verdict 'no'.
#iex(14)>

#iex(17)> send pid, {self, :stp}
#{#PID<0.106.0>, :stp}
#iex(18)> flush
#{:ok, -87}
#{:bye, 1}
#:ok

#iex(21)> {:ok, _, pid} = :monitor.start_online {Elixir.Demo.CalcServer, :start, [0]}, &:example_2.mfa_spec/1, []
#[INFO - <0.141.0> - tracer:234] - Started ROOT tracer <0.141.0> for top-level process <0.140.0>.
#*** [<0.141.0>] Instrumenting monitor for MFA pattern '{'Elixir.Demo.CalcServer',
#                                                                 loop,
#                                                                 [0]}'.
#{:ok, #PID<0.141.0>, #PID<0.142.0>}
#[INFO - <0.141.0> - tracer:754] - Instrumenting tracer <0.143.0> on MFA {'Elixir.Demo.CalcServer',loop,[0]} for process <0.142.0>.
#*** [<0.143.0>] Analyzing event {trace,<0.142.0>,spawned,<0.140.0>,
#                                      {'Elixir.Demo.CalcServer',loop,[0]}}.
#iex(22)> send pid, {self, {:add, 10, 97}}
#*** [<0.143.0>] Analyzing event {trace,<0.142.0>,'receive',
#                                      {<0.106.0>,{add,10,97}}}.
#{#PID<0.106.0>, {:add, 10, 97}}
#*** [<0.143.0>] Analyzing event {trace,<0.142.0>,send,{ok,107},<0.106.0>}.
#*** [<0.143.0>] Unfolding rec. var. 'X'.
#iex(23)>

