ERL = erl -boot drink -sname drink -mnesia dir "\"`pwd`/mnesia_data\"" -mnesia schema_location disc

all: app release

app: compile
	make -C lib/drink-1.0 compile

check: compile
	dialyzer -c ebin

compile:
	erl -make

release: drink.rel
	erlc -W drink.rel

clean:
	rm -f ebin/*.beam priv/epam
	rm -f drink.boot

run:
	$ERL
