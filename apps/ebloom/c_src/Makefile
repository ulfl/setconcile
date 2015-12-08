all:
	mkdir -p ../priv
	g++ ebloom_nifs.cpp -o ../priv/ebloom_nifs.so -fPIC -shared -I ${ERTS_INCLUDE_DIR} -L ${ERL_INTERFACE_LIB_DIR} -lstdc++ -lerl_interface -lei

clean:
	rm -f ebloom_nifs.o ../priv/ebloom_nifs.so

include env.mk
