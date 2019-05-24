#!/bin/bash

generate=1
run_norm=1                                 # run normal tests

DIFF="diff -bBy"                           # -b ignore whitespace, -B ignore blank lines, -y do side-by-side comparison

INPUT=test-data/input.tmp                  # name for input file
EXPECT=test-data/expect.tmp                # name for expected output file
ACTUAL=test-data/actual.tmp                # name for actual output file
DIFFOUT=test-data/diff.tmp                 # name for diff output file

function major_sep(){
    printf '%s\n' '====================================='
}
function minor_sep(){
    printf '%s\n' '-------------------------------------'
}

printf "Loading tests... "
source test_multimanager_data.sh
printf "%d tests loaded\n" "$T"

NTESTS=$T
NPASS=0

all_tests=$(seq $NTESTS)

# Check whether a single test is being run
single_test=$1
if ((single_test > 0 && single_test <= NTESTS)); then
    printf "Running single TEST %d\n" "$single_test"
    all_tests=$single_test
    NTESTS=1
else
    printf "Running %d tests\n" "$NTESTS"
fi

# printf "tests: %s\n" "$all_tests"
printf "\n"

# Run normal tests: capture output and check against expected
if [ "$run_norm" = "1" ]; then
    printf "RUNNING NORMAL TESTS\n"
    for i in $all_tests; do
        printf "TEST %2d %-18s : " "$i" "${tnames[i]}"
        FAIL="0"
        
        # Run the test

        # run program with given input
        printf "%s\n" "${input[i]}" | ./multimanager -echo >& $ACTUAL
        # generate expected output
        printf "%s\n" "${output[i]}" > $EXPECT

        # Check for output differences, print side-by-side diff if problems
        if ! $DIFF $EXPECT $ACTUAL > $DIFFOUT
        then
            printf "FAIL: Output Mismatch\n"
            minor_sep
            printf "INPUT:\n%s\n\n" "${input[i]}"
            printf "Side-by-side diff below\n"
            printf "  LEFT  COLUMN:   Expected Ouptut\n";
            printf "  RIGHT COLUMN:   Actual Ouptut\n";
            printf "  MIDDLE SYMBOLS: Indicate lines with differences\n";
            printf "\n";
            printf '**EXPECT**\t\t\t\t\t\t\t**ACTUAL**\n';
            cat $DIFFOUT
            if [ "$generate" == "1" ]; then
                printf "\nFULL ACTUAL\n"
                cat $ACTUAL
            fi
            minor_sep
            FAIL="1"
        fi

        if [ -n "${tfiles[i]}" ]; then                   # if there is an output file to check
            TEXPECT=test-data/texpect.txt                # name for expected output file
            TACTUAL="${tfiles[i]}"                       # name for actual output file
            TDIFFOUT=test-data/ttest.diff                # name for diff output file
            
            # generate expected output
            printf "%s\n" "${tfiles_expect[i]}" > $TEXPECT

            # Check for output differences, print side-by-side diff if problems
            if ! $DIFF $TEXPECT $TACTUAL > $TDIFFOUT
            then
                printf "FAIL: File %s incorrect\n" "$TACTUAL"
                minor_sep
                printf "INPUT:\n%s\n" "${input[i]}"
                printf "OUTPUT: EXPECT   vs   ACTUAL\n"
                cat $TDIFFOUT
                if [ "$generate" == "1" ]; then
                    printf "FULL ACTUAL\n"
                    cat $TACTUAL
                fi
                minor_sep
                FAIL="1"
            fi
        fi


        if (( FAIL == 0 )); then
            printf "OK\n"
            ((NPASS++))
        fi            
    done
    printf "\n"
fi

# ================================================================================

major_sep
printf "OVERALL:\n"
printf "%2d / %2d tests correct\n" "$NPASS" "$NTESTS"

