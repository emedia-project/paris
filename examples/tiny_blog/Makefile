ERL					?= erl
ERLC				= erlc
REL_DIR     = rel
NODE				= {{name}}
REL					= {{name}}
APPS        = apps
SCRIPT_PATH  := $(REL_DIR)/$(NODE)/bin/$(REL)
REBAR       = ./rebar

.PHONY: compile rel get-deps 

all: compile

compile: get-deps
	@$(REBAR) compile
	@$(REBAR) db-create texas=config/tiny_blog.config

get-deps:
	@$(REBAR) get-deps
	@$(REBAR) check-deps

clean:
	@$(REBAR) db-drop texas=config/tiny_blog.config
	@$(REBAR) clean
	rm -f erl_crash.dump

realclean: clean
	@$(REBAR) delete-deps

test:
	@$(REBAR) skip_deps=true eunit

rel: compile
	@$(REBAR) generate

doc:
	$(REBAR) skip_deps=true doc
	for app in $(APPS); do \
		cp -R apps/$${app}/doc doc/$${app}; \
	done;

dev:
	@erl -pa ebin include apps/*/ebin apps/*/include deps/*/ebin deps/*/include 

analyze: checkplt
	@$(REBAR) skip_deps=true dialyze

buildplt:
	@$(REBAR) skip_deps=true build-plt

checkplt: buildplt
	@$(REBAR) skip_deps=true check-plt
