# Copyright 2013 Erlware, LLC. All Rights Reserved.
#
# BSD License see COPYING

ERL = $(shell which erl)
ERL_VER = $(shell erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell)

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/ebin

REBAR=$(shell which rebar)

ifeq ($(REBAR),)
#$(error "Rebar not available on this system, try running make get-rebar")
REBAR=$(CURDIR)/rebar
endif

ERLWARE_COMMONS_PLT=$(CURDIR)/.erlware_commons_plt

DEPS_PLT=$(CURDIR)/.depsolver_plt

.PHONY: all compile doc clean test shell distclean pdf get-deps rebuild #dialyzer typer #fail on Travis.

all: deps compile

$(REBAR):
	wget https://github.com/rebar/rebar/wiki/rebar
	chmod a+x rebar

get-rebar: $(REBAR)

deps:
	$(REBAR) get-deps
	$(REBAR) compile

get-deps: $(REBAR)
	$(REBAR) get-deps
	$(REBAR) compile

compile: $(REBAR) deps
	$(REBAR) compile

doc: compile
	- $(REBAR) skip_deps=true doc

eunit: compile
	$(REBAR) skip_deps=true eunit

ct: compile clean-common-test-data
	mkdir -p $(CURDIR) logs
	ct_run -pa $(CURDIR)/ebin \
	-pa $(CURDIR)/deps/*/ebin \
	-logdir $(CURDIR)/logs \
	-dir $(CURDIR)/test/ \
	-suite basic_SUITE

$(DEPS_PLT):
	@echo Building local erts plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt \
	--apps erts kernel stdlib -r deps

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) \
	-Wrace_conditions -r ./ebin

shell: compile
# You often want *rebuilt* rebar tests to be available to the
# shell you have to call eunit (to get the tests
# rebuilt). However, eunit runs the tests, which probably
# fails (thats probably why You want them in the shell). This
# runs eunit but tells make to ignore the result.
	exec erl +K true -pa ebin -env ERL_LIBS deps -name erlastic@127.0.0.1 -s erlastic_search_app start_deps

clean-common-test-data:
# We have to do this because of the unique way we generate test
# data. Without this rebar eunit gets very confused
	- rm -rf $(CURDIR)/test/*_SUITE_data

clean: clean-common-test-data
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/logs
	- rm -rf $(CURDIR)/ebin
	$(REBAR) skip_deps=true clean

distclean: clean
	- rm -rf $(DEPS_PLT)
	- rm -rvf $(CURDIR)/deps/*

clean-deps: clean
	rm -rvf $(CURDIR)/deps/*
	rm -rf $(ERLWARE_COMMONS_PLT).$(ERL_VER)

rebuild: clean-deps get-deps all ct
