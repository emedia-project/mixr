REBAR = ./rebar
XREL  = ./xrel
RM_RF = rm -rf

.PHONY: compile get-deps test

all: compile

release: compile
	@$(XREL) tar

compile: get-deps
	@$(REBAR) compile

get-deps:
	@$(REBAR) get-deps
	@$(REBAR) check-deps

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

realclean: clean
	@$(REBAR) delete-deps
	@$(RM_RF) ebin
	@$(RM_RF) deps

doc: compile
	@rm -f documentation.md
	@rm -rf doc
	@$(REBAR) doc

test: compile
	@$(REBAR) skip_deps=true eunit

dev: compile
	@erl -pa ebin include deps/*/ebin deps/*/include -config config/mixr.config -args_file config/vm.args

