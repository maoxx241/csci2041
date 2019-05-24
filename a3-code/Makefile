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
	test_document \
	test_doccol \
	test_bulkops \


all : $(MODULES) $(PROGRAMS) 

util.cmo : util.ml
	$(OCC) -c $<

sortedlist.cmo : sortedlist.ml
	$(OCC) -c $<

document.cmo : document.ml
	$(OCC) -c $<

doccol.cmo : doccol.ml document.cmo
	$(OCC) -c $<

bulkops.cmo : bulkops.ml document.cmo doccol.cmo sortedlist.cmo util.cmo
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
test : test-p1 test-p2 test-p3 test-p4

mltest.cmo : mltest.ml
	$(OCC) -c $<

clean-tmp : test-data
	rm -f test-data/*.tmp

test-data :
	mkdir -p test-data

%.mlt : %.ml
	@chmod u+x process-mltest.awk
	./process-mltest.awk $< > $@


# BASELINE: sortedlist tests
test-sortedlist : test_sortedlist test-data clean-tmp
	@printf "===TESTS for sortedlist.cmo, insert / remove / print===\n"
	./test_sortedlist
	@printf "\n"

test_sortedlist : mltest.cmo sortedlist.cmo test_sortedlist.cmo 
	$(OCC) -o $@ $(LIBS) $^

test_sortedlist.cmo : test_sortedlist.mlt sortedlist.cmo mltest.cmo 
	$(OCC) -c $<


# PROBLEM 1
test-p1 : test_document clean-tmp
	@printf "===TESTS for document.cmo===\n"
	./test_document
	@printf "\n"

test_document : mltest.cmo document.cmo test_document.cmo 
	$(OCC) -o $@ $(LIBS) $^

test_document.cmo : test_document.mlt document.cmo mltest.cmo 
	$(OCC) -c $<

# PROBLEM 2
test-p2 : test_doccol clean-tmp
	@printf "===TESTS for doccol.cmo===\n"
	./test_doccol
	@printf "\n"

test_doccol : mltest.cmo document.cmo doccol.cmo test_doccol.cmo 
	$(OCC) -o $@ $(LIBS) $^

test_doccol.cmo : test_doccol.mlt doccol.cmo doccol.cmo mltest.cmo 
	$(OCC) -c $<

# PROBLEM 3
test-p3 : test_bulkops clean-tmp
	@printf "===TESTS for bulkops.cmo===\n"
	./test_bulkops
	@printf "\n"

test_bulkops : mltest.cmo util.cmo sortedlist.cmo document.cmo doccol.cmo bulkops.cmo test_bulkops.cmo 
	$(OCC) -o $@ $(LIBS) $^

test_bulkops.cmo : test_bulkops.mlt doccol.cmo doccol.cmo bulkops.cmo sortedlist.cmo util.cmo mltest.cmo 
	$(OCC) -c $<

# PROBLEM 4
test-p4 : multimanager clean-tmp
	@chmod u+x test_multimanager.sh
	@printf "===TESTS for multimanager===\n"
	./test_multimanager.sh
	@printf "\n"
