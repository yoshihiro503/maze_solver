open Util
open JSON
open Wget

type node = {name:string; f_count: int option}
type t = { id: string; pass: string; start: node; goal: node}

let snode node = match node.f_count with
| None -> !%":%s" node.name
| Some fc -> !%"%d:%s" fc node.name
let colon = Str.regexp ":"
let pnode s = 
  match Str.split colon s with
  | [nm] -> {name=nm; f_count=None}
  | [fc; nm] -> {name=nm; f_count=Some (int_of_string fc) }
  | _ -> failwith (!%"pnode(%s)" s)

let node_dec n1 n2 =
  n1.name=n2.name
let node_dec2 n1 n2 =
  print_endline (!%"node_dec '%s' '%s' : %b" n1.name n2.name (n1=n2));
  node_dec n1 n2


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
    let url = !%"twitter.com/statuses/friends.json?screen_name=%s&cursor=%s" node.name cursor in
    puts "sleep";
    Unix.sleep 30;
    let r = JSON.parse @@ Wget.wget ~user:m.id ~password:m.pass url in
    let nodeof obj =
      let sn = JSON.getf "screen_name" obj
	+> JSON.as_string
      in
      let fc = JSON.getf "friends_count" obj
	  +> JSON.as_int
      in
      {name=sn; f_count=Some fc}
    in
    let fs = List.fold_left (fun store j ->
      try nodeof j :: store with e -> puts (!%"NErr:"^Printexc.to_string e); store)
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
      puts (!%"next(%s) = [%s]" node.name (slist "," snode fs));
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


let nmemoise (f : 'a -> 'b) tblid skey serialize deserialize rvalidator =
  let tbl : ('b) Netshash.t =
    Netshash.create tblid serialize deserialize
  in
  fun x ->
    match Netshash.get tbl (skey x) with
    | Some (_, y) -> y
    | None ->
	let y = f x in
	if rvalidator x y then Netshash.add tbl (skey x) y;
	y

let delim = Str.regexp " "

let validator node nextnodes =
  let r = node.f_count = List.length nextnodes in
  if not r then puts (!%"validatorFalse!:%s" (snode node));
  r
    

let next = nmemoise next "tbl03"
    (fun key -> key.name)
    (fun ss -> !%"%d %s" (List.length ss) @@ slist " " snode ss)
    (fun s ->
      match Str.split delim s with
      | n :: ss -> List.map pnode ss
      | [] -> failwith "mustnothappen")
    validator

let next n =
  try
    memoise next n
  with
  | e -> print_endline ("ERR:" ^ Printexc.to_string e);
      []



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
  { id=twid; pass=ps; start={name=st;f_count=None}; goal={name=gl;f_count=None}}
    
let m = parse_input ()
let start : node
    = m.start
let goal : node
    = m.goal
