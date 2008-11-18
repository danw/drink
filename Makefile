ERL = erl -boot drink -sname drink -mnesia dir "\"`pwd`/mnesia_data\"" -mnesia schema_location disc -pz ebin

all: app release

app: compile
	true

check: compile
	dialyzer -c ebin

compile: priv/epam
	erl -make

release: compile drink.rel
	erlc -W -I ebin drink.rel

clean:
	rm -f ebin/*.beam priv/epam drink.boot drink.script erl_crash.dump

run: release
	${ERL}

priv/epam: src/pam/epam.c
	gcc -o priv/epam -lpam src/pam/epam.c -lerl_interface -lei -lpthread
