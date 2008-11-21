ERL = erl -boot drink -config drink -sname drink

all: app release

app: compile
	true

check: compile
	dialyzer -c ebin

drink.config:
	touch drink.config

compile: priv/epam
	erl -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.' -noshell

release: compile drink.rel
	erl -eval 'case systools:make_script("drink", [no_module_tests, local, {path, ["ebin", "/usr/local/lib/yaws/ebin"]}]) of ok -> halt(0); error -> halt(1) end.' -noshell

clean:
	rm -f ebin/*.beam priv/epam drink.boot drink.script erl_crash.dump

run: release drink.config
	mkdir -p log web_log
	${ERL}

priv/epam: src/pam/epam.c
	gcc -o priv/epam -lpam src/pam/epam.c -lerl_interface -lei -lpthread

logs:
	erl -boot start_sasl -sasl sasl_error_logger false -noshell -eval 'rb:start([{report_dir, "/root/drink/log"}]), rb:show(), init:stop().'