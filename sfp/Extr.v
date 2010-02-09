Require Import Util.
Require Import Solver.
Require Import List.

(* unit *)
Extract Inductive unit => "unit" ["()"].

(* pair *)
Extract Inductive prod => "(*)" [ "(,)" ].
Extract Constant fst => "fst".
Extract Constant snd => "snd".

(* bool *)
Extract Inductive bool => "bool" ["true" "false"].

(* sumbool *)
Extract Inductive sumbool => "bool" ["true" "false"].

(* list *)
Extract Inductive list => "list" ["[]" "(::)"].
Extract Constant map => "List.map".
Extract Constant flat_map => "(fun f xs -> List.flatten (List.map f xs))".

(* option *)
Extract Inductive option => "option" ["Some" "None" ].


Extract Constant maze => "Maze.t".
Extract Constant node => "Maze.node".
Extract Constant next => "Maze.next".
Extract Constant node_dec => "Maze.node_dec".
Extract Constant node_dec2 => "Maze.node_dec2".
Extract Constant start => "Maze.start".
Extract Constant goal  => "Maze.goal".

Extraction "solver.ml" goals.
