all: compile

compile:
	@./rebar skip_deps=true compile

compileall:
	@./rebar compile

clean:
	@./rebar skip_deps=true clean

console:
	@rel/sirenad/bin/sirenad console

test:
	@./rebar skip_deps=true eunit

analyze:
	@./rebar skip_deps=true analyze

checkplt:
	@./rebar skip_deps=true check_plt

buildplt:
	@./rebar skip_deps=true build_plt

xref:
	@./rebar skip_deps=true xref

rel: checkdeps compileall
	@cd rel; ../rebar generate

relclean:
	@rm -rf rel/sirenad

clobber: relclean clean

checkdeps:
	@./rebar skip_deps=ture check-deps

getdeps:
	@./rebar skip_deps=true get-deps

deldeps:
	@./rebar skip_deps=true delete-deps

.PHONY: all compile compileall clean console test analyze checkplt buildplt xref rel relclean getdeps deldeps
