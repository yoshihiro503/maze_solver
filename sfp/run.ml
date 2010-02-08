open Util
open Maze
open Solver

let rec path_iter f = function
  | PUnit x -> f x
  | PCons (x, p) -> f x; path_iter f p

let rec path_foldleft f y = function
  | PUnit x -> f y x
  | PCons (x,p) -> path_foldleft f (f y x) p

let string_of_path m path =
  slist " -> " id (path_foldleft (fun store n -> n :: store) [] path)
(*  path_iter (fun node -> print_string (!%" - %s" (snode node))) path;
  ""*)

let _ =
  let rec loop i gs =
    print_endline ("\n===");
    print_endline (!%"=LOOP %d:" i);
    match gs with
    | lazy (Cons ([], xs)) ->
	loop (i+1) xs
    | lazy (Cons (p::_, xs)) ->
	print_endline @@ string_of_path m p
  in
  loop 0 Solver.goals
