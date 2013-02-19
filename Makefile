.PHONY: rel deps test

all: deps compile

run: deps compile
	erl -sname macaba@localhost -config macaba_server.config -pa ebin apps/*/ebin deps/*/ebin -s macaba_app

runf: deps compilef
	erl -sname macaba@localhost -config macaba_server.config -pa ebin apps/*/ebin deps/*/ebin -s macaba_app

compilef:
	./rebar compile skip_deps=true

compile: rebar
	./rebar compile

deps: rebar
	./rebar get-deps

clean: rebar
	./rebar clean

test: compile
	@./rebar skip_deps=true eunit

rel: rebar deps
	./rebar compile generate -f

#devrel: $(DEVNODES)

#testrel: $(DEVNODES) $(TESTNODES)

COMBO_PLT = $(HOME)/.macaba_combo_dialyzer_plt
PLT_LIBS  = $(wildcard rel/moss/lib/*/ebin)

DIALYZER_APPS = macaba
DIALYZER_APPS_PATHS = $(addsuffix /ebin, $(addprefix apps/, $(DIALYZER_APPS)))

check_plt: rel
	dialyzer --check_plt --plt $(COMBO_PLT) $(PLT_LIBS)

build_plt: rel
	dialyzer --build_plt --output_plt $(COMBO_PLT) $(PLT_LIBS)

dialyzer: compile
	dialyzer -Wno_return --fullpath --plt $(COMBO_PLT) $(DIALYZER_APPS_PATHS) | \
	    fgrep -v -f ./dialyzer.ignore-warnings

cleanplt:
	rm $(COMBO_PLT)

#test_deps: rebar
#	./rebar -C rebar.tests.config get-deps

rebar:
	wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar

