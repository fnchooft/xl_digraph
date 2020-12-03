all: compile

compile:
	rebar3 compile
clean:
	rebar3 clean
	rm -fr logs
	rm -f test/*.beam
test: all
	rebar3 eunit
	rebar3 ct

eunit:
	rebar3 eunit suites=so_digraph,test_so_digraph

docs:
	rebar3 doc

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets\
xmerl snmp public_key mnesia eunit syntax_tools compiler eunit webtool

COMBO_PLT = $(HOME)/.xl_digraph_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \

dialyzer: all
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) ebin

cleanplt:
	@echo
	@echo "Are you sure? It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)
