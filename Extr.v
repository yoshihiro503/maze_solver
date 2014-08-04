Require Import Solver.
Require Import ExtrOcamlBasic.

Extract Constant maze => "Maze.t".
Extract Constant node => "Maze.node".
Extract Constant next => "Maze.next".
Extract Constant node_dec => "(=)".
Extract Constant start => "Maze.start".
Extract Constant goal  => "Maze.goal".

Extraction "solver.ml" goals.
