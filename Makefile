.SUFFIXES: .erl .beam

%.o: %.c
	erlc -W $<

ERL = erl -boot drink -sname drink -mnesia dir "\"`pwd`/mnesia_data\"" -mnesia schema_location disc

all: check run

run: drink.boot
	${ERL}

compile:
	erlc -W +debug_info -I /usr/lib/erlang/lib/stdlib-1.14.5/include/ *.erl pam/*.erl
	gcc -o epam -lpam pam/epam.c -lerl_interface -lei -lpthread

drink.boot: drink.rel compile
	erlc -W drink.rel

check: compile
	dialyzer -c .

clean:
	rm -rf *.beam pam/*.beam epam