#!/bin/bash

rm a.out

flex pankiewicza.l
g++ lex.yy.c
./a.out < sample_input.txt
