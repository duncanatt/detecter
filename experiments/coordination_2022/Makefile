################################################################################
## Emulation configurations                                                   ##
################################################################################


LOG_DIR=runs/coordination/logs


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
	erlc -pa $(BIN) +debug_info -W0 -I $(INCLUDE) -o $(BIN) $(call recursive,$(SRC),erl) $(call recursive,$(SRC2),erl)
	cp $(RES)/* $(BIN)

weave:
	erl -noshell -pa ebin -eval 'lin_weaver:weave("$(SRC)/ranch", fun prop_no_http_500_crash:mfa_spec/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}]).' -s init stop
	erl -noshell -pa ebin -eval 'lin_weaver:weave("$(SRC)/cowboy", fun prop_no_http_500_crash:mfa_spec/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}]).' -s init stop

run:
	mkdir -p $(LOG_DIR)
	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -noshell -eval '$(APP_CMD)' -s init stop

deploy-run:
	mkdir -p $(LOG_DIR)
	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -noshell -eval '$(APP_CMD)'

plot: run
	source $(VENV)/bin/activate; python $(SCRIPTS)/$(PLT_CMD)

clean:
	rm -rf $(BIN)/*.beam $(BIN)/*.E $(BIN)/*.tmp erl_crash.dump $(BIN)/*.app
