COMPFLAGS = -g -annot
OCC = ocamlc $(COMPFLAGS)
LIBS = str.cma unix.cma


PROGRAMS = \
	listmanager \


MODULES = \
	util.cmo \
	sortedlist.cmo \
	undolist.cmo \
	listmanager.cmo \

TEST_PROGRAMS = \


all : $(MODULES) $(PROGRAMS) 

util.cmo : util.ml
	$(OCC) -c $<

sortedlist.cmo : sortedlist.ml
	$(OCC) -c $<

undolist.cmo : undolist.ml
	$(OCC) -c $<

listmanager.cmo : listmanager.ml
	$(OCC) -c $<

listmanager : util.cmo sortedlist.cmo undolist.cmo listmanager.cmo
	$(OCC) -o $@ $(LIBS) $^

clean :
	rm -f *.cmo *.cmi *.mlt *.annot $(PROGRAMS) $(TEST_PROGRAMS)

########################################
# Testing Targets
# Will be released at a later time
