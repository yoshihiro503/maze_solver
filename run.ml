open Util
open Maze
open Solver

let rec path_iter f = function
  | PUnit x -> f x
  | PCons (x, p) -> f x; path_iter f p

let show path =
  let r = Array.copy maze in
  path_iter (fun idx -> if get r idx = ' ' then set r idx '$') path;
  print_endline @@ smatrix string1 r

let _ =
  let rec take1 = function
    | lazy (Cons ([], ps)) -> take1 ps
    | lazy (Cons (p::_, _)) -> p
  in
  show (take1 Solver.goals)

