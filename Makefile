ERL = erl -boot drink -sname drink -mnesia dir "\"`pwd`/mnesia_data\"" -mnesia schema_location disc 

all: app release

app: compile
	true

check: compile
	dialyzer -c ebin

compile: priv/epam
	erl -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.' -noshell

release: compile drink.rel
	erl -eval 'case systools:make_script("drink", [no_module_tests, local, {path, ["ebin"]}]) of ok -> halt(0); error -> halt(1) end.' -noshell

clean:
	rm -f ebin/*.beam priv/epam drink.boot drink.script erl_crash.dump

run: release
	${ERL}

priv/epam: src/pam/epam.c
	gcc -o priv/epam -lpam src/pam/epam.c -lerl_interface -lei -lpthread
