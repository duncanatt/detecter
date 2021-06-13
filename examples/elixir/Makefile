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

