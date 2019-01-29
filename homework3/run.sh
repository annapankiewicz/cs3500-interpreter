#!/bin/bash

rm a.out

flex minir.l
bison minir.y
g++ minir.tab.c
