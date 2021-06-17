################################################################################
## Project configurations                                                     ##
################################################################################

BIN=ebin

define recursive
	$(shell find $(1) -name "*.$(2)")
endef

all: clean

clean:
	rm -f $(BIN)/*.beam $(BIN)/*.E $(BIN)/*.erl $(BIN)/*.tmp $(BIN)/*.app erl_crash.dump
	rm -df $(BIN)

