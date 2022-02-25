################################################################################
## Project configurations                                                     ##
################################################################################

BIN=ebin
INCLUDE=include
SRC=src
TEST=test

define recursive
	$(shell find $(1) -name "*.$(2)")
endef

all: compile

clean:
	rm -f $(BIN)/*.beam $(BIN)/*.E $(BIN)/*.erl $(BIN)/*.tmp $(BIN)/*.app erl_crash.dump

compile: clean
	mkdir -p $(BIN)
	erlc -pa $(BIN) +debug_info -W0 -I $(INCLUDE) -o $(BIN) $(call recursive,$(SRC),erl)

compile-test: clean
	mkdir -p $(BIN)
	erlc -DTEST -pa $(BIN) +debug_info -I $(INCLUDE) -o $(BIN) $(call recursive,$(SRC),erl)
	erlc -DTEST -pa $(BIN) -I $(INCLUDE) -o $(BIN) $(call recursive,$(TEST),erl)

test: compile-test
	erl -noshell -pa $(BIN) -eval 'case eunit:test(log_tracer_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop
	#erl -noshell -pa $(BIN) -eval 'case eunit:test(tracer_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop

analyze:
	 dialyzer -pa $(BIN) -I $(INCLUDE) $(call recursive,$(SRC),erl)

test-loop: compile-test
	number=1 ; while [[ $$number -le 100 ]] ; do \
        echo "------------------------------------------------------------------------------------------> Running test #$$number" ; \
        erl -noshell -pa $(BIN) -eval 'case eunit:test(log_tracer_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop ; \
        ((number = number + 1)) ; \
    done

# dialyzer --build_plt --apps erts kernel stdlib mnesia compiler syntax_tools ssh public_key edoc parsetools