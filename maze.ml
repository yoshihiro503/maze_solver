open Util

let parse_input () =
  let rec iter store =
    try
      iter (explode (read_line()) :: store)
    with
    | End_of_file -> List.rev store
  in
  matrix_of_list @@ iter []

type t = char array array
type node = int * int

let snode (i,j) = "("^si i^","^si j^")"
let m = parse_input ()
let get : t -> node -> char
    = fun m (i,j) -> m.(j).(i)
let set : t -> node -> char -> unit
    = fun m (i,j) c -> m.(j).(i) <- c
let sidx (i,j) =
  si i ^ "," ^ si j

let width, height = (Array.length m.(0), Array.length m)
    
let start : node
    = matrix_find_idx ((=) 'S') m
let goal : node
    = matrix_find_idx ((=) 'G') m

let next : node -> node list 
    = fun (i,j) ->
      let avail m (x,y) =
	try
	  0 <= x && x < width && 0 <= y && y < height && get m (x,y) <> '*'
	with
	  e -> print_endline ("avail"^ si x ^"," ^si y); raise e
      in	
      List.filter (avail m)
      [(i-1,j); (i,j-1); (i+1,j); (i,j+1)]


