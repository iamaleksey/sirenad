.PHONY: all deps compile clean console rel relclean clobber test analyze buildplt xref

all: deps compile

deps:
	@./rebar get-deps

compile:
	@./rebar compile

clean:
	@./rebar clean

console:
	@rel/sirenad/bin/sirenad console

rel: deps compile relclean
	@cd rel; ../rebar generate

relclean:
	@rm -rf rel/sirenad

clobber: clean relclean
	@rm deps -rf

test:
	@./rebar skip_deps=true eunit

analyze:
	@./rebar skip_deps=true analyze

buildplt:
	@./rebar skip_deps=true build_plt

xref:
	@./rebar skip_deps=true xref
