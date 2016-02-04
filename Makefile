PROJECT = mixr

DEPS = lager bucs cowboy eredis 
dep_lager = git https://github.com/basho/lager.git master
dep_bucs = git https://github.com/botsunit/bucs.git master
dep_cowboy = git https://github.com/ninenines/cowboy.git master
dep_eredis = git https://github.com/wooga/eredis.git master
# mixr-plugins
# DEPS += mixr_freegeoip
# dep_mixr_freegeoip = git https://github.com/emedia-project/mixr_freegeoip.git master

DEP_PLUGINS = jorel
REL_DEPS = jorel
JOREL_MASTER = true
dep_jorel = git https://github.com/emedia-project/jorel.git master

DOC_DEPS = edown
dep_edown = git https://github.com/botsunit/edown.git master

include erlang.mk

EDOC_OPTS = {doclet, edown_doclet} \
						, {app_default, "http://www.erlang.org/doc/man"} \
						, {source_path, ["src"]} \
						, {overview, "overview.edoc"} \
						, {stylesheet, ""} \
						, {image, ""} \
						, {top_level_readme, {"./README.md", "https://github.com/emedia-project/${PROJECT}"}}

EUNIT_OPTS = verbose, {report, {eunit_surefire, [{dir, "test"}]}}

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include -config config/mixr.config -setcookie mixr -name mixr1@127.0.0.1

mixr1: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include -config config/mixr1.config -name mixr1@127.0.0.1 -setcookie mixr

mixr2: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include -config config/mixr2.config -name mixr2@127.0.0.1 -setcookie mixr

