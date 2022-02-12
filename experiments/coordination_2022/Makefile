################################################################################
## Emulation configurations                                                   ##
################################################################################


LOG_DIR=runs/fase/logs


### Cowboy experiments ###
APP_CMD=experiment_cowboy:start()

################################################################################
## Project configurations                                                     ##
################################################################################

BIN=ebin
INCLUDE=include
#INCLUDE2=../../detecter/include
SRC=src
SRC2=../../detecter/src
TEST=test
RES=resources

SCRIPTS=scripts
VENV=$(SCRIPTS)/venv

# Set shell to bash to use certain commands such as source.
SHELL=/bin/bash

define recursive
	$(shell find $(1) -name "*.$(2)")
endef

all: compile

compile: clean
	mkdir -p $(BIN)
	erlc -pa $(BIN) +debug_info -I $(INCLUDE) -o $(BIN) $(call recursive,$(SRC),erl) $(call recursive,$(SRC2),erl)
	cp $(RES)/* $(BIN)

compile-test: clean
	mkdir -p $(BIN)
	erlc -DTEST -pa $(BIN) +debug_info -I $(INCLUDE) -o $(BIN) $(call recursive,$(SRC),erl)
	erlc -DTEST -pa $(BIN) -I $(INCLUDE) -o $(BIN) $(call recursive,$(TEST),erl)

test: compile-test
	erl -noshell -pa ebin -eval 'eunit:test(log_tracer_test, [verbose])' -s init stop
	erl -noshell -pa ebin -eval 'eunit:test(async_tracer_test, [verbose])' -s init stop

weave: compile
# 	erl -noshell -pa ebin -eval 'weaver:weave("$(SRC)/models", "$(INCLUDE)", "$(BIN)", fun launcher:driver_monitors/1, fun launcher:filter_spec/1).' -s init stop
# 	erl -noshell -pa ebin -eval 'weaver:weave("$(SRC)/models", fun launcher:driver_monitors/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}, {filter, fun launcher:filter_spec/1}]).' -s init stop
# 	erl -noshell -pa ebin -eval 'weaver:weave("$(SRC)/models", fun experiment_dais:mon_log/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}, {filter, fun launcher:filter_spec/1}]).' -s init stop
#	erl -noshell -pa ebin -eval 'weaver:weave("$(SRC)/models", fun experiment_dais:driver_monitors/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}, {filter, fun launcher:filter_spec/1}]).' -s init stop
#	erl -noshell -pa ebin -eval 'weaver:weave("$(SRC)/models", fun experiment_oopsla:driver_monitors/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}, {filter, fun launcher:filter_spec/1}]).' -s init stop
#	erl -noshell -pa ebin -eval 'weaver:weave("$(SRC)/models", fun experiment_ifm:driver_monitors/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}, {filter, fun launcher:filter_spec/1}]).' -s init stop
	erl -noshell -pa ebin -eval 'weaver:weave("/Users/duncan/Dropbox/PhD/Development/detecter-inline/src/system", fun example_1:mfa_spec/1, [{outdir, "/Users/duncan/Dropbox/PhD/Development/detecter-inline/ebin"}, {i, "/Users/duncan/Dropbox/PhD/Development/detecter-inline/include"}, {filter, fun monitor:filter/1}, erl]).' -s init stop

weave-cowboy: compile
	#erl -noshell -pa ebin -eval 'hml_eval:compile("examples/cowboy_mon.hml", [{outdir, "ebin"}]).' -s init stop
	erl -noshell -pa ebin -eval 'lin_weaver:weave("$(SRC)/ranch", fun cowboy_mon:mfa_spec/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}]).' -s init stop
	erl -noshell -pa ebin -eval 'lin_weaver:weave("$(SRC)/cowboy", fun cowboy_mon:mfa_spec/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}]).' -s init stop

run:
	mkdir -p $(LOG_DIR)
	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -noshell -eval '$(APP_CMD)' -s init stop
# 	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -noshell -eval '$(APP_CMD)'
# 	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -eval '$(APP_CMD)'

run-forever:
	mkdir -p $(LOG_DIR)
	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -noshell -eval '$(APP_CMD)'

plot: run
	source $(VENV)/bin/activate; python $(SCRIPTS)/$(PLT_CMD)

load:
	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -eval 'code:ensure_modules_loaded([ascii_writer,async_mon,async_tracer_test,build,client,collector,common,csv_writer,distr,driver,echo_protocol,events,evm_tracer,gen_file_poller,gen_file_poller_impl,gen_looper,gen_looper_impl,hml_eval,hml_lexer,hml_lint,hml_parser,launcher,log,log_eval,log_lexer,log_parser,log_poller,log_tracer,main,master,monitor,opts,server,slave,stats,system,trace_lib,tracer,tracer_monitor,util,weaver]).'

clean:
	rm -rf $(BIN)/*.beam $(BIN)/*.E $(BIN)/*.tmp erl_crash.dump $(BIN)/*.app
