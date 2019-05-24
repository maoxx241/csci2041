# Mon Sep 10 10:35:16 CDT 2018 :: minor update to ensure that the
# process-mltest.awk is executable

COMPFLAGS = -g -annot
OCC = ocamlc $(COMPFLAGS)

PROGRAMS = \


MODULES = \
	sumfuncs.cmo \
	revfuncs.cmo \
	abovefuncs.cmo \


TEST_PROGRAMS = \
	test_sumfuncs \
	test_abovefuncs \
	test_revfuncs \


all : $(MODULES) $(PROGRAMS) 

sumfuncs.cmo : sumfuncs.ml
	$(OCC) -c $<

revfuncs.cmo : revfuncs.ml
	$(OCC) -c $<

abovefuncs.cmo : abovefuncs.ml
	$(OCC) -c $<

clean :
	rm -f *.cmo *.cmi *.mlt *.annot $(PROGRAMS) $(TEST_PROGRAMS)

########################################
# Testing Targets
test : test-p1 test-p2 test-p3

mltest.cmo : mltest.ml
	$(OCC) -c $<

# PROBLEM 1
test-p1 : test_sumfuncs
	@printf "===TESTS for P1 sumfuncs.cmo===\n"
	./test_sumfuncs
	@printf "\n"

test_sumfuncs : mltest.cmo sumfuncs.cmo test_sumfuncs.cmo 
	$(OCC) -o $@ $^

test_sumfuncs.cmo : test_sumfuncs.mlt sumfuncs.cmo mltest.cmo 
	$(OCC) -c $<

test_sumfuncs.mlt : test_sumfuncs.ml
	chmod u+x ./process-mltest.awk && ./process-mltest.awk $< > $@

# PROBLEM 2
test-p2 : test_abovefuncs
	@printf "===TESTS for P2 abovefuncs.cmo===\n"
	./test_abovefuncs
	@printf "\n"

test_abovefuncs : mltest.cmo abovefuncs.cmo test_abovefuncs.cmo 
	$(OCC) -o $@ $^

test_abovefuncs.cmo : test_abovefuncs.mlt abovefuncs.cmo mltest.cmo 
	$(OCC) -c $<

test_abovefuncs.mlt : test_abovefuncs.ml
	chmod u+x ./process-mltest.awk && ./process-mltest.awk $< > $@


# PROBLEM 3
test-p3 : test_revfuncs
	@printf "===TESTS for P3 revfuncs.cmo===\n"
	./test_revfuncs
	@printf "\n"

test_revfuncs : mltest.cmo revfuncs.cmo test_revfuncs.cmo 
	$(OCC) -o $@ $^

test_revfuncs.cmo : test_revfuncs.mlt revfuncs.cmo mltest.cmo 
	$(OCC) -c $<

test_revfuncs.mlt : test_revfuncs.ml
	chmod u+x ./process-mltest.awk && ./process-mltest.awk $< > $@