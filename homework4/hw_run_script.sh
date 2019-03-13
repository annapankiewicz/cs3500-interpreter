#!/bin/bash

# To run:
#	bash hw_run_script.sh

# This script assumes you have compiled your solution and called
# the executable "parser", as shown:

#	flex <filename>.l
#	bison <filename>.y
#	g++ <filename>.tab.c -o parser

# This script also assumes you have the provided sample input files from
# Canvas in a directory called "sample_input". It also requires an
# empty directory named "actual_output".
# The script will produce a side-by-side comparison of your output
# to the expected output. Any lines that have a "|", "<", or ">"
# mark differences between the files.

inputs=`ls sample_input --ignore-backups`

for i in $inputs; do
    ./parser < ./sample_input/$i \
	     > ./actual_output/$i.out

    diff ./actual_output/$i.out ./expected_output/$i.out > \
         ./reports//$i --ignore-space-change --ignore-case \
         --side-by-side --ignore-blank-lines
done
