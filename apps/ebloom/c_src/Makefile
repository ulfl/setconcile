all:
	mkdir -p ../priv
	g++ ebloom_nifs.cpp -o ../priv/ebloom_nifs.so -fPIC -shared -I /usr/lib/erlang/usr/include/ -L /usr/lib/erlang/lib/erl_interface-3.7.20/lib -lstdc++ -lerl_interface -lei
