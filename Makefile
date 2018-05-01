##
## Command line utility to stream stdin / stdout via sockets
APP = socat
ORG = fogfish
URI = 

app: all 
	./rebar3 escriptize

include erlang.mk


