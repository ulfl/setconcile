PROJECT = setconcile

DEPS = lager config ebloom cowboy hackney riak_pb riakc

dep_lager    = git https://github.com/basho/lager.git 2.1.1
dep_config   = git https://github.com/ulfl/config.git 02535d4
dep_ebloom   = git https://github.com/basho/ebloom.git 2.0.0
dep_cowboy   = git https://github.com/ninenines/cowboy.git 1.0.1
dep_hackney  = git https://github.com/benoitc/hackney.git 1.1.0
dep_riak_pb  = git https://github.com/basho/riak_pb.git 1.3.3slf
dep_riakc    = git https://github.com/basho/riak-erlang-client.git 1.3.3slf

ERLC_OPTS = +debug_info +"{parse_transform, lager_transform}"

include erlang.mk
