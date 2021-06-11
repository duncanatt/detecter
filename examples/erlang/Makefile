################################################################################
## Project configurations                                                     ##
################################################################################

BIN=ebin
INCLUDE=include
SRC=src

define recursive
	$(shell find $(1) -name "*.$(2)")
endef

all: compile

clean:
	rm -f $(BIN)/*.beam $(BIN)/*.E $(BIN)/*.erl $(BIN)/*.tmp $(BIN)/*.app erl_crash.dump
	rm -df $(BIN)

compile: clean
	mkdir -p $(BIN)
	erlc -pa $(BIN) +debug_info -I $(INCLUDE) -o $(BIN) $(call recursive,$(SRC),erl)