PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

.PHONY: all edoc test clean build_plt dialyzer app

all:
	@$(REBAR) prepare-deps

edoc: all
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) eunit

clean:
	@$(REBAR) clean
