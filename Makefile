PROJECT = mixr

DEPS = lager eutils cowboy eredis mixr_freegeoip

dep_lager = git https://github.com/basho/lager.git master
dep_eutils = git https://github.com/emedia-project/eutils.git master
dep_cowboy = git https://github.com/ninenines/cowboy.git master
dep_eredis = git https://github.com/wooga/eredis.git master
# mixr-plugins
dep_mixr_freegeoip = git https://github.com/emedia-project/mixr_freegeoip.git master

DEP_PLUGINS = jorel
BUILD_DEPS = jorel

dep_jorel = git https://github.com/emedia-project/jorel.git master

include erlang.mk

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include -config config/mixr.config

