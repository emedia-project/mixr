REBAR = ./rebar
XREL  = ./xrel

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

test: compile
	@$(REBAR) skip_deps=true eunit

dev: compile
	@erl -pa ebin include deps/*/ebin deps/*/include -config config/mixr.config -args_file config/vm.args

analyze: checkplt
	@$(REBAR) skip_deps=true dialyze

buildplt:
	@$(REBAR) skip_deps=true build-plt

checkplt: buildplt
	@$(REBAR) skip_deps=true check-plt
