open Util

type node = string
type t = { id: string; pass: string; start: node; goal: node}

let parse_input () =
  prerr_string "target: @";
  let gl = read_line() in
  prerr_string "your id: @";
  let twid = read_line() in
  prerr_string "password: ";
  let ps = read_line() in
  { id=twid; pass=ps; start=twid; goal=gl}
    
let snode node = "@"^node
let m = parse_input ()

(*
let get : t -> node -> char
    = fun m (i,j) -> m.(j).(i)
let set : t -> node -> char -> unit
    = fun m (i,j) c -> m.(j).(i) <- c
let sidx (i,j) =
  si i ^ "," ^ si j
*)
(*let width, height = (Array.length m.(0), Array.length m)*)
    
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


