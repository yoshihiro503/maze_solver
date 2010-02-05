open Util
open JSON

type node = string
type t = { id: string; pass: string; start: node; goal: node}

let parse_input () =
  prerr_string "target: @"; flush stderr;
  let gl = read_line() in
  prerr_string "your id: @"; flush stderr;
  let twid = read_line() in
  prerr_string "password: "; flush stderr;
  let ps = read_line() in
  print_endline "";
  print_endline ("parsed: target=@"^gl);
  { id=twid; pass=ps; start=twid; goal=gl}
    
let snode node = "@"^node
let m = parse_input ()
let node_dec n1 n2 =
  print_endline (!%"node_dec '%s' '%s' : %b" n1 n2 (n1=n2));
  n1=n2

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
    = m.start
let goal : node
    = m.goal

(* WGET
let fd = Unix.socket PF_INET SOCK_STREAM 0 in
Unix.connect fd (ADDR_INET (Unix.inet_addr_of_string "twitter.com/hoghoge", 80));
let ch = in_channel_of_descr fd in
read_all ch
*)

let wget ?(user="") ?(password="") url : string =
  let query =
    !%"wget --user=\"%s\" --password=\"%s\" %s -O -" user password url
  in
  let ch = Unix.open_process_in query in
  let lines = value @@ tee (fun _ -> close_in ch) @@ maybe read_all ch in
  slist "\n" id lines


let followers node : node list =
(*  let url = "twitter.com/statuses/followers.json?screen_name="^node in*)
  let url = "twitter.com/statuses/friends.json?screen_name="^node in
  let r = JSON.parse @@ wget ~user:m.id ~password:m.pass url in
  let screen_name obj =
(*    JSON.getf "user" obj
      +>*)
    JSON.getf "screen_name" obj
      +> JSON.to_string
  in
  List.fold_left (fun store node ->
    try screen_name node :: store with _ -> store)
    [] @@ JSON.to_list r

let next : node -> node list 
    = fun node ->
      try
  let fs = followers node in
  prerr_endline (!%"next(%s) = [%s]" node (slist "," id fs));
  print_endline (!%"next(%s) = [%s]" node (slist "," id fs));
  fs
      with
      | e ->
	  print_endline ("ERR:" ^ Printexc.to_string e);
	  []
(*
      match node with
      | "yoshihiro503" -> ["mzp"; "shimomura1004"; "keigoi"]
      | "mzp" -> ["yoshihiro503"; "nobio0205"]
      | _ -> ["yoshihiro503"]
*)

let memoise (f : 'a -> 'b) =
  let tbl : ('a, 'b) Hashtbl.t = Hashtbl.create 100 in
  fun x ->
    if Hashtbl.mem tbl x then
      Hashtbl.find tbl x
    else
      let y = f x in
      Hashtbl.add tbl x y;
      y
	  
let next = memoise next
