.PHONY: all clean opt

all: laby robot.cmo
opt: laby.opt

VERSION:=$(shell sh config.sh)

OCAMLFIND=ocamlfind
OPTIONS=-package getopt,lablgtk2

OCAML_VERSION=$(shell $(OCAML) -version)
OCAMLC=$(OCAMLFIND) ocamlc $(OPTIONS) -custom -g
OCAMLOPT=$(OCAMLFIND) ocamlopt $(OPTIONS) -unsafe
OCAMLDEP=$(OCAMLFIND) ocamldep $(OPTIONS)
OCAMLLEX=ocamllex
MENHIR=menhir --no-stdlib

-include .depend

S=run.cmo config.cmo f.cmo opt.cmo version.cmo
BASE_S=state.cmo gfx.cmo laby.cmo

LABY_S=$(S) $(BASE_S)

BASE_LIBS=unix.cma nums.cma getopt.cma
GFX_LIBS=bigarray.cma lablgtk.cma
LABY_LIBS=$(BASE_LIBS) $(GFX_LIBS)

laby: $(LABY_S)
	$(OCAMLC) $(LABY_LIBS) $^ -o $@

laby.opt: $(LABY_S:.cmo=.cmx)
	$(OCAMLOPT) $(LABY_LIBS:.cma=.cmxa) $^ -o $@

clean:
	rm -rf *.cm[iox] *.o laby{,.opt}
	rm -rf config.ml
	rm -rf .depend

%.cmi: %.mli
	$(OCAMLC) -c $<
%.cmo: %.ml
	$(OCAMLC) -c $<
%.cmx: %.ml
	$(OCAMLOPT) -c $<

.depend: $(wilcard *.ml *.mli)
	@$(OCAMLDEP) *.ml{,i} >> .depend
