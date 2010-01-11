ERL = erl -boot drink -config drink -sname drink +K true
YAWS_EBIN = /usr/local/lib/yaws/ebin
PWD := $(shell pwd)
HOSTNAME := $(shell hostname)

INCLUDES := /usr/local/include
LIB_PATHS :=
ERL_LIBS := erl_interface ei pthread 

ifeq ($(shell uname -s),Darwin)
  INCLUDES += /opt/local/lib/erlang/lib/erl_interface-3.6.1/include \
              /usr/include/pam
  LIB_PATHS += /opt/local/lib/erlang/lib/erl_interface-3.6.1/lib
endif

ifeq ($(shell uname -s),Linux)
  INCLUDES += /usr/include/security
endif

ifeq ($(HOSTNAME),erlang.csh.rit.edu)
  HOSTNAME := drink.csh.rit.edu
endif

all: app release

app: compile
	true

check: compile
	dialyzer -c ebin

drink.config: drink.config.in
	m4 --define=PATH=$(PWD) --define=HOSTNAME=$(HOSTNAME) drink.config.in >drink.config

drink.rel:
	cp drink.rel.sample drink.rel

compile: priv/epam priv/ewebauth
	erl -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.' -noshell

release: compile drink.rel
	erl -eval 'case systools:make_script("drink", [no_module_tests, local, {path, ["ebin", "$(YAWS_EBIN)"]}]) of ok -> halt(0); error -> halt(1) end.' -noshell

clean:
	rm -f ebin/*.beam priv/epam priv/ewebauth drink.boot drink.script erl_crash.dump drink.config MnesiaCore.*

run: release drink.config
	mkdir -p log web_log
	$(ERL)

priv/epam: src/pam/epam.c
	gcc -o priv/epam -lpam src/pam/epam.c $(addprefix -l,$(ERL_LIBS)) $(addprefix -I,$(INCLUDES)) $(addprefix -L,$(LIB_PATHS))

priv/ewebauth: src/webauth/ewebauth.c
	gcc -o priv/ewebauth src/webauth/ewebauth.c -lwebauth $(addprefix -l,$(ERL_LIBS)) $(addprefix -I,$(INCLUDES)) $(addprefix -L,$(LIB_PATHS))

logs:
	erl -boot start_sasl -sasl sasl_error_logger false -noshell -eval 'rb:start([{report_dir, "$(PWD)/log"}]), rb:show(), init:stop().'

shell:
	erl -sname drink_shell
