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
	rm -f $(BIN)/*.beam $(BIN)/*.E $(BIN)/*.tmp erl_crash.dump $(BIN)/*.app
	rm -df $(BIN)

compile: clean
	mkdir -p $(BIN)
	erlc -pa $(BIN) +debug_info -I $(INCLUDE) -o $(BIN) $(call recursive,$(SRC),erl)

# Steps

# 1. Compile: make

# 2. Start shell: erl -pa ebin

# 3. Run the function hello.

# 4. Quit shell: q().

# Steps

# 1. Compile: make

# 2. Start shell: erl -pa ebin -pa ../../detecter/ebin

# 3. Compile analyzer: hml_eval:compile("props/hello_prop.hml", [{outdir,"ebin"}, v]).

# 4. hello_prop:mfa_spec({hello, greet, ["Duncan"]}).

# 4. monitor:start_online({hello,start_greet, ["Duncan"]}, fun hello_prop:mfa_spec/1, []).

