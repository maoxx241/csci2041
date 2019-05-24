#!/usr/bin/awk -f
#
# usage: process-mltest.awk testfile.ml > testfile.mlt
#        process-mltest.awk -vDEBUG=1 testfile.ml > testfile.mlt
#
# This script adds processes an OCaml source file to replace any
# __check__() calls with exception throws that transmit information
# about the location and error type. It is meant to be used in testing
# files to produce nicer looking output to help with debugging test
# results.

# Print debugging info; enable with -vDEBUG=1
function debug_print(str){
  if(DEBUG != ""){
    printf("DEBUG %s:%d: %s\n",FILENAME,FNR,str);
  }
}

BEGIN{
  in_test = 0                   # not in a test yet
  test_code = ""                # no code is part of the test yet
}

/(* BEG_TEST *)/{               # begining of test
  in_test = 1
  test_code = ""                # reset test code
  debug_print("begin test")
}

/(* END_TEST *)/{               # end of test
  in_test = 0
  test_code = ""                # reset test code
  debug_print("end test")
}

# Matched a line that is in a test
in_test==1 && $0 !~ /(* BEG_TEST *)/{
  line_esc = $0                 # escape the backslashes and other special chars
  gsub("\\\\","\\\\",line_esc)  # that are in the current line of code
  gsub("\"","\\\"",line_esc)    
  line_esc = FNR ":" line_esc
  test_code = test_code line_esc "\\n" # add on the escaped code to the current test code
  debug_print("in test, adding : " $0)
}

# Found a check() call, replace it with exception raise and
# contextural info on where the test contents. Look for a variable
# "msg" which will contain the specific message.
/^ *__check__/{
  debug_print("check found: " $0)
  $1=""                         # eliminate check() call
  gsub("; *$","")               # eliminate trailing semicolon
  check_code=$0                 # capture the code of the check() call
  loc=FILENAME ":" FNR          # construct code location

  # sub in values include the accumulated test_code variable which has
  # all the test code for the current test so far
  fmt = "if not (%s) then raise (TestFail(\"%s\",msg,\"%s\")) else ();"
  $0 = sprintf(fmt,check_code,loc,test_code)
}

{ print }
