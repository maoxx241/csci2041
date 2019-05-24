COMPFLAGS = -g -annot
OCC = ocamlc $(COMPFLAGS)
LIBS = str.cma unix.cma


PROGRAMS = \
	multimanager \


MODULES = \
	util.cmo \
	sortedlist.cmo \
	document.cmo \
	bulkops.cmo \
	multimanager.cmo \


TEST_PROGRAMS = \
	test_sortedlist1 \
	test_sortedlist2 \
	test_undolist \


all : $(MODULES) $(PROGRAMS) 

util.cmo : util.ml
	$(OCC) -c $<

sortedlist.cmo : sortedlist.ml
	$(OCC) -c $<

document.cmo : document.ml
	$(OCC) -c $<

doccol.cmo : doccol.ml document.cmo
	$(OCC) -c $<

bulkops.cmo : bulkops.ml document.cmo doccol.cmo sortedlist.cmo util.cmo   #util.cmo added at end
	$(OCC) -c $<

multimanager.cmo : multimanager.ml util.cmo sortedlist.cmo document.cmo bulkops.cmo doccol.cmo
	$(OCC) -c $<

multimanager : util.cmo sortedlist.cmo document.cmo doccol.cmo bulkops.cmo multimanager.cmo 
	$(OCC) -o $@ $(LIBS) $^

clean :
	rm -f *.cmo *.cmi *.mlt *.annot $(PROGRAMS) $(TEST_PROGRAMS) 
	rm -f test-data/*.diff test-data/*.actual test-data/*.expect test-data/*.tmp

########################################
# Testing Targets
# Will be released at a later time
