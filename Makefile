PROJECT = setconcile

DEPS = lager ebloom cowboy shotgun

dep_lager    = git https://github.com/basho/lager.git 2.1.1
dep_ebloom   = git https://github.com/basho/ebloom.git 2.0.0
dep_cowboy   = git https://github.com/ninenines/cowboy.git 1.0.1
dep_lhttpc   = git https://github.com/inaka/shotgun.git 0.1.6

ERLC_OPTS = +debug_info +"{parse_transform, lager_transform}"

include erlang.mk
