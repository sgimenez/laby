.PHONY: all clean opt

all: laby robot
opt: laby.opt

VERSION:=$(shell sh config.sh)

OCAMLFIND=ocamlfind
OPTIONS=-package getopt,lablgtk2

OCAML_VERSION=$(shell $(OCAML) -version)
OCAMLC=$(OCAMLFIND) ocamlc $(OPTIONS) -custom -g
OCAMLOPT=$(OCAMLFIND) ocamlopt $(OPTIONS) -unsafe
OCAMLMKTOP=$(OCAMLFIND) ocamlmktop $(OPTIONS)
OCAMLDEP=$(OCAMLFIND) ocamldep $(OPTIONS)

-include .depend

S=run.cmo config.cmo f.cmo opt.cmo version.cmo

LABY_S=$(S) state.cmo gfx.cmo laby.cmo

LABY_LIBS=unix.cma nums.cma getopt.cma bigarray.cma lablgtk.cma

laby: $(LABY_S)
	$(OCAMLC) $(LABY_LIBS) $^ -o $@

laby.opt: $(LABY_S:.cmo=.cmx)
	$(OCAMLOPT) $(LABY_LIBS:.cma=.cmxa) $^ -o $@

robot: robot.cmo
	$(OCAMLMKTOP) -o $@ unix.cma $^


srobot.cmo: srobot.ml
	$(OCAMLC) -pp camlp4rf -I +camlp4 -c $<
srobot.cmx: srobot.ml
	$(OCAMLOPT) -pp camlp4rf -I +camlp4 -c $<

srobot: robot.cmo srobot.cmo
	$(OCAMLMKTOP) -o $@ unix.cma $^

clean:
	rm -rf *.cm[iox] *.o laby{,.opt} robot srobot
	rm -rf config.ml
	rm -rf .depend

%.cmi: %.mli
	$(OCAMLC) -c $<
%.cmo: %.ml
	$(OCAMLC) -c $<
%.cmx: %.ml
	$(OCAMLOPT) -c $<

.depend: config.ml f.ml gfx.ml laby.ml opt.ml robot.ml run.ml state.ml version.ml $(wildcard *.mli)
	@$(OCAMLDEP) $^ >> .depend
