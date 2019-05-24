                               __________

                                A4 TESTS
                               __________


Automated Testing Instructions
==============================

  Unzip `a4-tests.zip' to create the directory `a4-tests/'.  Copy all
  files in the `a4-tests' directory to the project directory
  `a4-code'. The following unix command should do this if `a4-tests' and
  a4-code are in the same directory.

  ,----
  | cp -r a4-tests/* a4-code/
  `----

  The Makefile provided is a copy of Makefile.withtests. The original
  version is called Makefile.notests.

  The new Makefile introduces the targets
  - `make test-p1' : test problem 1
  - `make test-p2' : test problem 2
  - `make test-p3' : test problem 3
  - `make test' : test all problems

  Test programs `test_ssmap', `test_treemap', `test_map_modules', and
  `test_treeset' associated with problems 1-3 can run individual tests
  by passing a test number as a command line argument as in

  ,----
  | ./test_treeset 3    # run the 3rd test only
  `----
