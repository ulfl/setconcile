set -e

make -C apps/bear eunit
#make -C apps/config eunit # no tests
#make -C apps/cowboy eunit # build failure
#make -C apps/cowlib eunit # build failure
make -C apps/ebloom eunit
make -C apps/erlcron eunit
#make -C apps/erlcron eunit # build failure
make -C apps/goldrush eunit
#make -C apps/hackney eunit # build failure
make -C apps/idna eunit
make -C apps/jsx eunit
#make -C apps/lager eunit # build failure
#make -C apps/meck eunit # build failure
make -C apps/ranch eunit
#make -C apps/ssl_verify_hostname eunit # some failures
