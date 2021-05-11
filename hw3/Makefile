OCAMLC = ocamlopt
CMO = cmx
CMA = cmxa

LIBS = str.$(CMA)

javascript:	flags.$(CMO) javascript_utils.$(CMO) javascript_ast.$(CMO) javascript_parser.$(CMO) javascript_lexer.$(CMO) testing.$(CMO) homework03.$(CMO) javascript_main.$(CMO) main.$(CMO)
	$(OCAMLC) -o javascript $(LIBS) flags.$(CMO) javascript_utils.$(CMO) javascript_ast.$(CMO) javascript_parser.$(CMO) javascript_lexer.$(CMO) testing.$(CMO) homework03.$(CMO) javascript_main.$(CMO) main.$(CMO)

main.$(CMO):	main.ml homework03.$(CMO) testing.$(CMO) javascript_main.$(CMO) javascript_parser.$(CMO) javascript_lexer.$(CMO) javascript_ast.$(CMO) javascript_utils.$(CMO) flags.$(CMO)
	$(OCAMLC) -c main.ml

flags.$(CMO):	flags.ml
	$(OCAMLC) -c flags.ml

testing.$(CMO):	testing.ml
	$(OCAMLC) -c testing.ml

homework03.$(CMO):	homework03.ml testing.$(CMO)
	$(OCAMLC) -c homework03.ml

hw03:	javascript
		./javascript -hw03

javascript_Makefile:	javascript.gra
	uruz2 javascript.gra

clean:	javascript_clean
	rm -rf *.o *.cm* *.mli javascript_parser.ml javascript_lexer.ml

-include javascript_Makefile
