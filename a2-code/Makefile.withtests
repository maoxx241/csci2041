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
	test_sortedlist1 \
	test_sortedlist2 \
	test_undolist \


all : $(MODULES) $(PROGRAMS) 

util.cmo : util.ml
	$(OCC) -c $<

sortedlist.cmo : sortedlist.ml
	$(OCC) -c $<

undolist.cmo : undolist.ml
	$(OCC) -c $<

# ppexec.cmo : ppexec.ml
# 	$(OCC) -c $<

listmanager.cmo : listmanager.ml
	$(OCC) -c $<

listmanager : util.cmo sortedlist.cmo undolist.cmo listmanager.cmo
	$(OCC) -o $@ $(LIBS) $^

clean :
	rm -f *.cmo *.cmi *.mlt *.annot $(PROGRAMS) $(TEST_PROGRAMS) 
	rm -f test-data/*.diff test-data/*.actual test-data/*.expect test-data/*.tmp

########################################
# Testing Targets
test : test-p1 test-p2 test-p3 test-p4

mltest.cmo : mltest.ml
	$(OCC) -c $<

test-data :
	mkdir -p test-data
	rm -f test-data/*.tmp

# PROBLEM 1
test-p1 : test_sortedlist1 test-data
	@printf "===TESTS for sortedlist.cmo, insert / remove / print===\n"
	./test_sortedlist1
	@printf "\n"

test_sortedlist1 : mltest.cmo sortedlist.cmo test_sortedlist1.cmo 
	$(OCC) -o $@ $(LIBS) $^

test_sortedlist1.cmo : test_sortedlist1.mlt sortedlist.cmo mltest.cmo 
	$(OCC) -c $<

test_sortedlist1.mlt : test_sortedlist1.ml
	./process-mltest.awk $< > $@

# PROBLEM 2
test-p2 : test_sortedlist2
	@printf "===TESTS for sortedlist.cmo, merge===\n"
	./test_sortedlist2
	@printf "\n"

test_sortedlist2 : mltest.cmo sortedlist.cmo test_sortedlist2.cmo 
	$(OCC) -o $@ $(LIBS) $^

test_sortedlist2.cmo : test_sortedlist2.mlt sortedlist.cmo mltest.cmo 
	$(OCC) -c $<

test_sortedlist2.mlt : test_sortedlist2.ml
	./process-mltest.awk $< > $@

# PROBLEM 3
test-p3 : test_undolist
	@printf "===TESTS for undolist.cmo===\n"
	./test_undolist
	@printf "\n"

test_undolist : mltest.cmo sortedlist.cmo undolist.cmo test_undolist.cmo 
	$(OCC) -o $@ $(LIBS) $^

test_undolist.cmo : test_undolist.mlt sortedlist.cmo undolist.cmo mltest.cmo 
	$(OCC) -c $<

test_undolist.mlt : test_undolist.ml
	./process-mltest.awk $< > $@


# PROBLEM 4
test-p4 : listmanager
	@printf "===TESTS for listmanager===\n"
	./test_listmanager.sh
	@printf "\n"
