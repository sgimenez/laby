.PHONY: all clean opt

all: fdls
opt: fdls.opt

VERSION:=$(shell sh config.sh)

OCAMLFIND=ocamlfind
OPTIONS=-package getopt,lablgtk2,cairo

OCAML_VERSION=$(shell $(OCAML) -version)
OCAMLC=$(OCAMLFIND) ocamlc $(OPTIONS) -custom -g
OCAMLOPT=$(OCAMLFIND) ocamlopt $(OPTIONS) -unsafe
OCAMLDEP=$(OCAMLFIND) ocamldep $(OPTIONS)
OCAMLLEX=ocamllex
MENHIR=menhir --no-stdlib

-include .depend

S=run.cmo config.cmo f.cmo opt.cmo version.cmo
BASE_S=state.cmo gfx.cmo fdls.cmo

FDLS_S=$(S) $(BASE_S)

BASE_LIBS=unix.cma nums.cma getopt.cma
GFX_LIBS=bigarray.cma lablgtk.cma cairo.cma cairo_lablgtk.cma
FDLS_LIBS=$(BASE_LIBS) $(GFX_LIBS)

fdls: $(FDLS_S)
	$(OCAMLC) $(FDLS_LIBS) $^ -o $@

fdls.opt: $(LDLS_S:.cmo=.cmx)
	$(OCAMLOPT) $(FDLS_LIBS:.cma=.cmxa) $^ -o $@

clean:
	rm -rf *.cm[iox] *.o fdls{,.opt}
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
