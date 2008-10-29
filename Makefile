.SUFFIXES: .erl .beam

%.o: %.c
	erlc -W $<

ERL = erl -boot start_sasl

all: check run

run:
	${ERL}

compile:
	erlc -W +debug_info *.erl

check: compile
	dialyzer -c .

clean:
	rm -rf *.beam