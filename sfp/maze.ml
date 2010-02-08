open Util
open JSON
open Wget

type node = string
type t = { id: string; pass: string; start: node; goal: node}

let parse_input () =
  prerr_string "your id: @"; flush stderr;
  let twid = read_line() in
  prerr_string "password: "; flush stderr;
  let ps = read_line() in
  prerr_string "start: @"; flush stderr;
  let st = read_line() in
  prerr_string "target: @"; flush stderr;
  let gl = read_line() in
  prerr_endline "";
  puts ("parsed: target=@"^gl);
  { id=twid; pass=ps; start=st; goal=gl}
    
let snode node = "@"^node
let m = parse_input ()
let node_dec n1 n2 =
  print_endline (!%"node_dec '%s' '%s' : %b" n1 n2 (n1=n2));
  n1=n2

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

(*
let wget ?(user="") ?(password="") url : string =
  let query =
    !%"wget --user=\"%s\" --password=\"%s\" %s -O -" user password url
  in
  let ch = Unix.open_process_in query in
  let lines = value @@ tee (fun _ -> close_in ch) @@ maybe read_all ch in
  slist "\n" id lines
*)

let merge l1 l2 =
  let rec iter store = function
    | [] -> store
    | x::xs -> iter
	  (if List.mem x store then store else x :: store) xs
  in
  iter l2 l1

let friends node : node list =
(*  let url = "twitter.com/statuses/followers.json?screen_name="^node in*)
  let rec loop total cursor =
    let url = !%"twitter.com/statuses/friends.json?screen_name=%s&cursor=%s" node cursor in
    puts "sleep";
    Unix.sleep 30;
    let r = JSON.parse @@ Wget.wget ~user:m.id ~password:m.pass url in
    let screen_name obj =
      JSON.getf "screen_name" obj
	+> JSON.as_string
    in
    let fs = List.fold_left (fun store node ->
      try screen_name node :: store with _ -> store)
	[] @@ JSON.as_list (JSON.getf "users" r)
    in
    let next_cursor =
      JSON.getf "next_cursor_str" r
	+> JSON.as_string
    in
    let total' = merge fs total in
    if List.length fs < 100 then total'
    else loop (total') (next_cursor)
  in
  loop [] "-1"

let next : node -> node list 
    = fun node ->
      let fs = friends node in
      puts (!%"next(%s) = [%s]" node (slist "," id fs));
      fs
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


let nmemoise (f : 'a -> 'b) tblid serialize deserialize =
  let tbl : ('b) Netshash.t =
    Netshash.create tblid serialize deserialize
  in
  fun x ->
    match Netshash.get tbl x with
    | Some (_, y) -> y
    | None ->
	let y = f x in
	Netshash.add tbl x y;
	y

let delim = Str.regexp " "

let next = nmemoise next "tbl02"
    (fun ss -> !%"%d %s" (List.length ss) @@ slist " " id ss)
    (fun s ->
      match Str.split delim s with
      | n :: ss -> ss
      | [] -> failwith "mustnothappen")

let next n =
  try
    memoise next n
  with
  | e -> print_endline ("ERR:" ^ Printexc.to_string e);
      []
