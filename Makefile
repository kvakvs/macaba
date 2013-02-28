all: deps compile
MRUNCMD=erl -sname macaba@localhost -config macaba_erlang_node.config \
	-pa ebin apps/*/ebin deps/*/ebin \
	-s macaba_app -mnesia dir '"database/"'

.PHONY: run
run: deps compile database
	$(MRUNCMD)

.PHONY: runf
runf: compilef database
	$(MRUNCMD)

database:
	mkdir database

.PHONY: compile
compile: rebar
	./rebar compile

.PHONY: compilef
compilef:
	./rebar compile skip_deps=true

deps: rebar
	./rebar get-deps

.PHONY: clean
clean: rebar
	./rebar clean

.PHONY: test
test:
	@./rebar skip_deps=true eunit

.PHONY: rel
rel: rebar deps
	./rebar compile generate -f

#devrel: $(DEVNODES)

#testrel: $(DEVNODES) $(TESTNODES)

COMBO_PLT = $(HOME)/.macaba_combo_dialyzer_plt
PLT_LIBS  = $(wildcard rel/moss/lib/*/ebin)

DIALYZER_APPS = macaba
DIALYZER_APPS_PATHS = $(addsuffix /ebin, $(addprefix apps/, $(DIALYZER_APPS)))

.PHONY: check_plt
check_plt: rel
	dialyzer --check_plt --plt $(COMBO_PLT) $(PLT_LIBS)

.PHONY: build_plt
build_plt: rel
	dialyzer --build_plt --output_plt $(COMBO_PLT) $(PLT_LIBS)

.PHONY: dialyzer
dialyzer: compile
	dialyzer -Wno_return --fullpath --plt $(COMBO_PLT) $(DIALYZER_APPS_PATHS) | \
	    fgrep -v -f ./dialyzer.ignore-warnings

.PHONY: cleanplt
cleanplt:
	rm $(COMBO_PLT)

#test_deps: rebar
#	./rebar -C rebar.tests.config get-deps

rebar:
	wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar
