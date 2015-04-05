PROJECT = setconcile

DEPS = lager config ebloom cowboy shotgun lhttpc

dep_lager    = git https://github.com/basho/lager.git 2.1.1
dep_config   = git https://github.com/ulfl/config.git 23c03df
dep_ebloom   = git https://github.com/basho/ebloom.git 2.0.0
dep_cowboy   = git https://github.com/ninenines/cowboy.git 1.0.1
dep_shotgun  = git https://github.com/inaka/shotgun.git 0.1.8
dep_lhttpc   = git /Users/ulf/git/lhttpc c609177

ERLC_OPTS = +debug_info +"{parse_transform, lager_transform}"

include erlang.mk
