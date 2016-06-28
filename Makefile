OCAMLC=ocamlc
OCAML_FLAGS=-g

TARGET=test

MAIN_OBJS=ast.cmi ast.cmo parser.cmi parser.cmo lexer.cmo test.cmo

$(TARGET): $(MAIN_OBJS)
	$(OCAMLC) -o $@ $(OCAML_FLAGS) ast.cmo parser.cmo lexer.cmo test.cmo

lexer.ml: lexer.mll
	ocamllex $<

parser.ml: parser.mly
	ocamlyacc $<

parser.mli: parser.mly
	ocamlyacc $<

%.cmo: %.ml
	$(OCAMLC) $(OCAML_FLAGS) -c $<


%.cmi: %.mli
	$(OCAMLC) $(OCAML_FLAGS) -c $<

clean:
	rm -f *~ *.cm[iox] $(TARGET) parser.ml parser.mli lexer.ml
