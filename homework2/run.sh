#!/bin/bash

rm a.out
rm lex.yy.c
rm *.tab.c

flex minir.l
bison minir.y
g++ minir.tab.c
