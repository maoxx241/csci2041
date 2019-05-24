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
	test_document \
	test_doccol \
	test_bulkops \


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
