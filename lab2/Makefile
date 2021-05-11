OCAMLC = ocamlopt
CMO = cmx
CMA = cmxa
MLI = types

LIBS = str.$(CMA)

TARGETS = lab2
ASSIGNMENTS = lab2
ASSIGNMENTS_CMO = $(ASSIGNMENTS:=.$(CMO))
ASSIGNMENTS_ML = $(ASSIGNMENTS:=.ml)
ASSIGNMENTS_MLI = $(ASSIGNMENTS:=.$(MLI))
UTILS = testing javascript_main javascript_ast
UTILS_CMO = $(UTILS:=.$(CMO))

javascript:	flags.$(CMO) javascript_utils.$(CMO) javascript_ast.$(CMO) javascript_parser.$(CMO) javascript_lexer.$(CMO) testing.$(CMO) javascript_main.$(CMO) $(ASSIGNMENTS_CMO) main.$(CMO) $(ASSIGNMENTS_MLI)
	$(OCAMLC) -o javascript $(LIBS) flags.$(CMO) javascript_utils.$(CMO) javascript_ast.$(CMO) javascript_parser.$(CMO) javascript_lexer.$(CMO) testing.$(CMO) javascript_main.$(CMO) $(ASSIGNMENTS_CMO) main.$(CMO)

main.$(CMO):	main.ml $(ASSIGNMENTS_CMO) testing.$(CMO) javascript_main.$(CMO) javascript_parser.$(CMO) javascript_lexer.$(CMO) javascript_ast.$(CMO) javascript_utils.$(CMO) flags.$(CMO)
	$(OCAMLC) -c main.ml

flags.$(CMO):	flags.ml
	$(OCAMLC) -c flags.ml

testing.$(CMO):	testing.ml
	$(OCAMLC) -c testing.ml

.SECONDEXPANSION:
$(ASSIGNMENTS_CMO): $$(subst $(CMO),ml,$$@) $(UTILS_CMO) #%.ml $(UTILS_CMO)
	$(OCAMLC) -c $(subst $(CMO),ml,$@)

%.$(MLI): %.ml %.$(CMO)
	$(OCAMLC) -i $< > $@

#%.$(CMO): %.ml $(UTILS_CMO)
#	@echo building $<
#	$(OCAMLC) -c $<

$(TARGETS):	javascript
		./javascript -test -$@

mli:  $(ASSIGNMENTS_MLI)

javascript_Makefile:	javascript.gra
	uruz2 javascript.gra

clean:	javascript_clean
	rm -rf *.o *.cm* *.mli javascript_parser.ml javascript_lexer.ml

-include javascript_Makefile
