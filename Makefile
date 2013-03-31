MRUNCMD := erl -sname macaba@localhost -config macaba_erlang_node.config \
	-pa ebin apps/*/ebin apps/*/src deps/*/ebin \
	-s macaba_app -mnesia dir '"database/"'
HERE := $(shell pwd)
JITIFY := scripts/jitify-core-0.9.4

.PHONY: all
all: deps compile

##################### Minification - how to use $make minify ###################
#TEMPLATES_SRC := apps/mcweb/priv/tpl
#TEMPLATES := $(wildcard $(TEMPLATES_SRC)/*.dtl) $(wildcard $(TEMPLATES_SRC)/*.html)
#TEMPLATES_MINI := $(TEMPLATES_SRC)-mini
#TEMPLATES_OUT := $(subst $(TEMPLATES_SRC),$(TEMPLATES_MINI),$(TEMPLATES))
#${TEMPLATES_OUT}: ; $(JITIFY)/build/jitify --minify --html $(TEMPLATES_SRC)/$(notdir $@) > $@
#.PHONY: minify
#minify: jitify $(TEMPLATES_MINI) ${TEMPLATES_OUT}
#	@echo ------ Minification finished ------
#$(TEMPLATES_MINI):
#	mkdir $(TEMPLATES_MINI) $(TEMPLATES_MINI)/ebin $(TEMPLATES_MINI)/custom_tags
#.PHONY: jitify
#jitify: ; cd $(JITIFY) && make tools
#####################
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

.PHONY: deps
deps: rebar
	$(HERE)/rebar get-deps

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

OTP_PLT   = $(HOME)/.macaba_otp.plt
COMBO_PLT = $(HOME)/.macaba_combo.plt
PLT_LIBS0 = $(wildcard apps/*/ebin) $(wildcard deps/*/ebin)
PLT_LIBS  = $(subst deps/riak_pb/ebin,,$(PLT_LIBS0))

DIALYZER_APPS = macaba mcweb
DIALYZER_APPS_PATHS = $(addsuffix /ebin, $(addprefix apps/, $(DIALYZER_APPS)))

.PHONY: check_plt
check_plt: rel
	dialyzer --check_plt --plt $(COMBO_PLT) $(PLT_LIBS)

.PHONY: build_sysplt
build_sysplt: $(OTP_PLT)

$(OTP_PLT):
	dialyzer --output_plt $(OTP_PLT) --build_plt \
		--apps erts kernel stdlib mnesia compiler syntax_tools runtime_tools \
		crypto tools inets sasl ssh ssl public_key xmerl

.PHONY: build_plt
build_plt: compile build_sysplt $(COMBO_PLT)

$(COMBO_PLT):
	dialyzer --plt $(OTP_PLT) --output_plt $(COMBO_PLT) --add_to_plt $(PLT_LIBS)

.PHONY: dialyzer
dialyzer: compile check_plt
	dialyzer -Wno_return --fullpath --plt $(COMBO_PLT) $(DIALYZER_APPS_PATHS) \
	    | fgrep -v -f ./dialyzer.ignore-warnings | tee dialyzer.log -

.PHONY: cleanplt
cleanplt:
	rm $(COMBO_PLT)

#test_deps: rebar
#	./rebar -C rebar.tests.config get-deps

rebar:
	wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar
