COMPFLAGS = -g -annot
OCC = ocamlc $(COMPFLAGS)
LIBS = str.cma unix.cma


PROGRAMS = \
	ssmap_demo \
	treemap_demo \
	treeset_demo \


MODULES = \
	ssmap.cmo \
	treemap.cmo \
	map_modules.cmo \
	treeset.cmo \


TEST_PROGRAMS = \
	test_ssmap \
	test_treemap \
	test_map_modules \
	test_treeset \


all : $(MODULES) $(PROGRAMS) 

ssmap.cmo : ssmap.ml
	$(OCC) -c $<

ssmap_demo : ssmap.cmo ssmap_demo.ml
	$(OCC) -o $@ $^

treemap.cmo : treemap.ml
	$(OCC) -c $<

map_modules.cmo : map_modules.ml treemap.cmo
	$(OCC) -c $<

treemap_demo : treemap.cmo map_modules.cmo treemap_demo.ml
	$(OCC) -o $@ $^

treeset.cmo : treeset.ml treemap.cmo
	$(OCC) -c $<

treeset_demo : treemap.cmo treeset.cmo treeset_demo.ml
	$(OCC) -o $@ $^

clean :
	rm -f *.cmo *.cmi *.mlt *.annot $(PROGRAMS) $(TEST_PROGRAMS) 

########################################
# Testing Targets
test : test-p1 test-p2 # test-p3

mltest.cmo : mltest.ml
	$(OCC) -c $<

clean-tmp : test-data
	rm -f test-data/*.tmp

test-data :
	mkdir -p test-data

%.mlt : %.ml			#implicit rule for all .mlt files
	@chmod u+x process-mltest.awk
	./process-mltest.awk $< > $@

# PROBLEM 1
test-p1 : test_ssmap clean-tmp
	@printf "\n===TESTS for ssmap.cmo===\n"
	./test_ssmap
	@printf "\n"

test_ssmap : mltest.cmo ssmap.cmo test_ssmap.cmo 
	$(OCC) -o $@ $(LIBS) $^

test_ssmap.cmo : test_ssmap.mlt ssmap.cmo mltest.cmo 
	$(OCC) -c $<


# PROBLEM 2
test-p2 : test_treemap test_map_modules clean-tmp
	@printf "\n===TESTS for treemap.cmo===\n"
	./test_treemap
	@printf "\n===TESTS for map_modules.cmo===\n"
	./test_map_modules
	@printf "\n"

test_treemap : mltest.cmo treemap.cmo test_treemap.cmo 
	$(OCC) -o $@ $(LIBS) $^

test_treemap.cmo : test_treemap.mlt treemap.cmo mltest.cmo 
	$(OCC) -c $<

test_map_modules : mltest.cmo treemap.cmo map_modules.cmo test_map_modules.cmo 
	$(OCC) -o $@ $(LIBS) $^

test_map_modules.cmo : test_map_modules.mlt treemap.cmo map_modules.cmo mltest.cmo 
	$(OCC) -c $<

# PROBLEM 3
test-p3 : test_treeset 
	@printf "\n===TESTS for treeset.cmo===\n"
	./test_treeset

test_treeset : mltest.cmo treemap.cmo treeset.cmo test_treeset.cmo 
	$(OCC) -o $@ $(LIBS) $^

test_treeset.cmo : test_treeset.mlt treemap.cmo treeset.cmo mltest.cmo 
	$(OCC) -c $<
