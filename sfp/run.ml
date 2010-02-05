open Util
open Maze
open Solver

let rec path_iter f = function
  | PUnit x -> f x
  | PCons (x, p) -> f x; path_iter f p

let string_of_path m path =
  path_iter (fun node -> print_string (!%" - %s" (snode node))) path;
  ""

let _ =
  let rec loop i gs =
    match gs with
    | lazy (Cons ([], xs)) ->
	loop (i+1) xs
    | lazy (Cons (p::_, xs)) ->
	print_endline @@ string_of_path m p
  in
  loop 0 Solver.goals
