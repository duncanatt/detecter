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


# Running the Outline example.

## 1. Start the erlang shell.
# Start directory: detecter/examples/python
# erl -pa ../../detecter/ebin

## 2. Compile the property.
# hml_eval:compile("props/example_2.hml", [{outdir, "ebin"}, v]).

## 3. Run the outline monitor from the Erlang console (start dir: examples/python).
# It's ok for the log file not to exist. The file poller will pick it automatically once it does.
# monitor:start_offline("trace.log", pid(0,102,0), fun example_2:mfa_spec/1, []).

## 4. Run the Python script (start dir: examples/python/src).
# python -m demo.calc_server 0 ../trace.log

## Remember to stop the monitor because the log is read in a forward-only manner,
## so if we rewrite it we disrupt the parsing mechanism.
# monitor:stop().

