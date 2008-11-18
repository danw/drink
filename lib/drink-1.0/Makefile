ERL = erl -boot drink -sname drink -mnesia dir "\"`pwd`/mnesia_data\"" -mnesia schema_location disc -pz ebin

all: compile check run

run: compile
	${ERL}

compile: src/drink.rel
	erl -make
	erlc -W src/drink.rel

check: compile
	@dialyzer -c .

clean:
	rm -rf ebin/*.beam priv/epam ebin/*.boot ebin/*.script
