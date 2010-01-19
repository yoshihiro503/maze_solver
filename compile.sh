#!/bin/sh
coqc Util.v Solver.v Extr.v
ocamlopt -c util.ml maze.ml
ocamlc -c solver.mli
ocamlopt util.ml maze.ml solver.ml run.ml -o run
rm -f *.cm[iox] *.o *.vo *.glob
