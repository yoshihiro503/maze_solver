#!/bin/sh
coqc Util.v Solver.v Extr.v
ocamlopt -c str.cmxa unix.cmxa util.ml wget.ml llist.ml parserMonad.ml jSON.ml netshash.ml maze.ml
ocamlc -c solver.mli
ocamlopt str.cmxa unix.cmxa util.ml wget.ml llist.ml parserMonad.ml jSON.ml netshash.ml maze.ml solver.ml run.ml -o smaze
rm -f *.cm[iox] *.o *.vo *.glob
