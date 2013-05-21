REBAR = $(shell pwd)/rebar

.PHONY: deps rel package

all: deps compile

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

distclean: clean
	$(REBAR) delete-deps

test: all
	$(REBAR) skip_deps=true eunit

###
### Docs
###
docs:
	$(REBAR) skip_deps=true doc

##
## Developer targets
##

xref:
	$(REBAR) xref skip_deps=true

console: all
	erl -pa ebin deps/*/ebin -s dhcp -config standalone.config


##
## Dialyzer
##
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.dhcp_combo_dialyzer_plt


check_plt: deps compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin ebin

build_plt: deps compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin ebin

dialyzer: deps compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) deps/*/ebin ebin | grep -v -f dialyzer.mittigate


cleanplt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)
