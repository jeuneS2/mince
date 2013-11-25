CAMLC=ocamlc
CAMLOPT=ocamlopt -p
CAMLLEX=ocamllex
CAMLDEP=ocamldep
CAMLDOC=ocamldoc
COMPFLAGS=-warn-error A -dtypes -g -I +ocamlgraph
CAMLYACC=ocamlyacc
YACCFLAGS=-v
DOCFLAGS=-html -d camldoc

COMPOBJS=location.cmo spec.cmo math.cmo util.cmo \
	lexer.cmo parser.cmo \
	options.cmo \
	cliques.cmo ilpgen.cmo \
	etoile.cmo

OPTOBJS=$(COMPOBJS:.cmo=.cmx)

SOURCES=$(COMPOBJS:.cmo=.ml)

all: compilo

opt: compiloopt

doc: compilo
	$(CAMLDOC) $(DOCFLAGS) $(SOURCES)

# The compiler

compilo: $(COMPOBJS)
	$(CAMLC) -o etoile -g nums.cma ocamlgraph/graph.cma $(COMPOBJS)

compiloopt: $(OPTOBJS)
	$(CAMLOPT) -o etoile.opt nums.cmxa ocamlgraph/graph.cmxa $(OPTOBJS)

# The parser

parser.mli parser.ml: parser.mly
	$(CAMLYACC) $(YACCFLAGS) parser.mly

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

# Misc

clean:
	rm -f *.cm[iox] *.[so] *~ *.annot .depend
	rm -f parser.ml parser.mli lexer.ml parser.output etoile etoile.opt

.depend:  parser.mli parser.ml lexer.ml $(SOURCES)
	$(CAMLDEP) $(DEPFLAGS) *.mli *.ml > .depend

-include .depend
