#!/bin/sh
coqc Util.v Solver.v Extr.v
ocamlopt -c unix.cmxa util.ml putget.ml llist.ml parserMonad.ml jSON.ml nethash.ml maze.ml
ocamlc -c solver.mli
ocamlopt unix.cmxa util.ml putget.ml llist.ml parserMonad.ml jSON.ml nethash.ml maze.ml solver.ml run.ml -o run
rm -f *.cm[iox] *.o *.vo *.glob
