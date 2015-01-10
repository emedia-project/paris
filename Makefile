REBAR = ./rebar

.PHONY: compile get-deps test doc

all: compile doc

compile: get-deps
	@$(REBAR) compile

get-deps:
	@$(REBAR) get-deps
	@$(REBAR) check-deps

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

realclean: clean
	@$(REBAR) delete-deps

test: compile
	@$(REBAR) skip_deps=true eunit

doc:
	@rm -f documentation.md
	@rm -rf doc
	@./make_doc

dev:
	@erl -pa ebin include deps/*/ebin deps/*/include -config config/samsa.config

