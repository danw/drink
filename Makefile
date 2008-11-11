.SUFFIXES: .erl .beam

%.o: %.c
	erlc -W $<

ERL = erl -boot drink -sname drink

all: check run

run: drink.boot
	${ERL}

compile:
	erlc -W +debug_info *.erl

drink.boot: drink.rel compile
	erlc -W drink.rel

check: compile
	dialyzer -c .

clean:
	rm -rf *.beam
