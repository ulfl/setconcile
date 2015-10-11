PROJECT = setconcile

# Versions used for local (inlined) deps: lager: 2.1.1, config 6c08948,
# ebloom: 2.0.0.
LOCAL_DEPS = lager config ebloom

DEPS  = cowboy hackney meck bear folsom jsx erlcron
DEPS += riak_pb riakc

dep_cowboy   = git https://github.com/ninenines/cowboy.git 1.0.3
dep_hackney  = git https://github.com/benoitc/hackney.git 1.1.0
dep_meck     = git https://github.com/eproxus/meck.git 0.8.2
dep_bear     = git https://github.com/boundary/bear.git 0.8.2
dep_folsom   = git https://github.com/boundary/folsom.git 0.8.2
dep_jsx      = git https://github.com/talentdeficit/jsx.git 50eff66
dep_erlcron  = git https://github.com/erlware/erlcron.git ac49936
dep_riak_pb  = git https://github.com/basho/riak_pb.git 1.4.4.0
dep_riakc    = git https://github.com/basho/riak-erlang-client.git 1.4.2

ERLC_OPTS = +debug_info +"{parse_transform, lager_transform}"

include erlang.mk
