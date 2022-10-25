#! /bin/bash

find $(cd .. && pwd) -name *.expand | xargs egypt | dot -Grotate=90 -Tps -o callgraph.ps
