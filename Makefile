.SUFFIXES: .erl .beam

%.o: %.c
	erlc -W $<

ERL = erl -boot drink -sname drink -mnesia dir "\"`pwd`/mnesia_data\"" -mnesia schema_location disc -pz epam eldap

all: check run

run: drink.boot
	${ERL}

compile:
	make epam
	erl -make
	erlc -W +debug_info -I /usr/lib/erlang/lib/stdlib-1.14.5/include/ *.erl

drink.boot: drink.rel compile
	erlc -W drink.rel

check: compile
	dialyzer -c .

clean:
	rm -rf ebin/*.beam priv/epam ebin/*.boot ebin/*.script
