PROJECT = setconcile

# Versions used for local (inlined) deps:
# * lager: 2.1.1
# * config ee6bd88.
# * ebloom: 2.0.0.
# * jsx 50eff66 (v2.8.0).
# * erlcron ac49936.
# * Folsom 0.8.2, bear 0.8.2, meck 0.8.2.
LOCAL_DEPS = lager config ebloom jsx erlcron folsom bear meck

DEPS  = cowboy hackney riak_pb riakc

dep_cowboy   = git https://github.com/ninenines/cowboy.git 1.0.3
dep_hackney  = git https://github.com/benoitc/hackney.git 1.1.0
dep_riak_pb  = git https://github.com/basho/riak_pb.git 1.4.4.0
dep_riakc    = git https://github.com/basho/riak-erlang-client.git 1.4.2

ERLC_OPTS = +debug_info +"{parse_transform, lager_transform}"

include erlang.mk
