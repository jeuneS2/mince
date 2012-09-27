CAMLC=ocamlc
CAMLOPT=ocamlopt -p
CAMLLEX=ocamllex
CAMLDEP=ocamldep
CAMLDOC=ocamldoc
COMPFLAGS= -warn-error A -dtypes -g
CAMLYACC=ocamlyacc
YACCFLAGS=-v
DOCFLAGS=-html -d camldoc

COMPOBJS=location.cmo spec.cmo math.cmo util.cmo \
	parser.cmo lexer.cmo \
	options.cmo \
	topsort.cmo graphgen.cmo \
	mince.cmo

OPTOBJS=$(COMPOBJS:.cmo=.cmx)

SOURCES=$(COMPOBJS:.cmo=.ml)

all: compilo
#	cp preludec "$(HOME)/bin"

opt: compiloopt
#	cp preludec.opt "$(HOME)/bin"

doc: compilo
	$(CAMLDOC) $(DOCFLAGS) $(SOURCES)

# The compiler

compilo: $(COMPOBJS)
	 $(CAMLC) -o mince -g nums.cma $(COMPOBJS)

compiloopt: $(OPTOBJS)
	   $(CAMLOPT) -o mince.opt nums.cmxa $(OPTOBJS)

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
	$(CAMLOPT)  -c $<

# Misc

clean:
	rm -f *.cm[iox] *.[so] *~ *.annot .depend
	rm -f parser.ml parser.mli lexer.ml parser.output mince mince.opt

.depend:  parser.mli parser.ml lexer.ml $(SOURCES)
	$(CAMLDEP) $(DEPFLAGS) *.mli *.ml > .depend

-include .depend
