ERL = erl -boot drink -sname drink -mnesia dir "\"`pwd`/mnesia_data\"" -mnesia schema_location disc

all: app release

app: compile
	true

check: compile
	dialyzer -c ebin

compile:
	erl -make

release: drink.rel
	erlc -W -I ebin drink.rel

clean:
	rm -f ebin/*.beam priv/epam drink.boot drink.script

run:
	$ERL
