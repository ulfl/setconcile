PROJECT = setconcile

# Versions used for local (inlined) deps:
# * lager: 3.0.2. (https://github.com/basho/lager.git)
#    goldrush 0.1.7
# * config ee6bd88. (https://github.com/ulfl/config)
# * ebloom: 2.0.0. (https://github.com/basho/ebloom.git)
# * jsx 50eff66 (v2.8.0). (https://github.com/talentdeficit/jsx.git)
# * erlcron ac49936. (https://github.com/erlware/erlcron.git)
# * Folsom
#    folsom 0.8.2. (https://github.com/boundary/folsom.git)
#    bear 0.8.2.
#    meck 0.8.2.
# * Cowboy
#    cowboy 1.0.3
#    cowlib 1.0.1
#    ranch  1.1.0
# * Hackney
#    hackney 1.3.2
#    ssl_verify_hostname 1.0.5
#    erlang-idna 1.0.2
LOCAL_DEPS  = lager config ebloom jsx erlcron folsom bear meck
LOCAL_DEPS += ranch cowlib cowboy ssl_verify_hostname idna hackney

DEPS = riak_pb riakc
dep_riak_pb  = git https://github.com/basho/riak_pb.git 1.4.4.0
dep_riakc    = git https://github.com/basho/riak-erlang-client.git 1.4.2

ERLC_OPTS = +debug_info -Werror +"{parse_transform, lager_transform}"

DIALYZER_DIRS = ./ebin
DIALYZER_OPTS = 

include erlang.mk
