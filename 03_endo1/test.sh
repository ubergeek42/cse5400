#!/bin/bash

ghc -O2 Main.hs
time ./a.out < $1 > $1.out
cat $1.out | sort | uniq -c | sort -r -n
rm $1.out
