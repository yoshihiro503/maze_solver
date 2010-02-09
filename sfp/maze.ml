open Util
open JSON
open Wget

type node = string
type t = { id: string; pass: string; start: node; goal: node}

let snode node = !%"%s" node

let pnode s = 
  s

let node_dec n1 n2 =
(*  n1.uid=n2.uid*)
  n1=n2
let node_dec2 n1 n2 =
(*  print_endline (!%"node_dec '%s' '%s' : %b" n1.uid n2.uid (n1=n2));*)
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

(*let node_of_obj obj =
(*  let fc = JSON.getf "friends_count" obj +> as_int in*)
  let id = JSON.getf "id" obj +> as_float
      +> Int64.of_float +> Int64.to_string in
  { uid=id; (*f_count=fc*) }*)

(*let node_of_name name =
  let url =
    !%"http://twitter.com/statuses/user_timeline.json?screen_name=%s&count=1" name
  in
  Unix.sleep 30;
  JSON.parse (Wget.wget url)
    +> (List.hd $ JSON.as_list)
    +> JSON.getf "user"
    +> node_of_obj*)

let twitter (twid,ps) cmd =
  let url = "twitter.com/" ^ cmd in
  puts "sleep:";
  Unix.sleep 1;
  JSON.parse (Wget.wget ~user:twid ~password:ps url)

let json_as_int64 : JSON.t -> string =
  Int64.to_string $ Int64.of_float $ JSON.as_float

let profile_of_id acc idorname =
  let u =
    twitter acc (!%"users/show/%s.json" idorname)
  in
  let fc = JSON.getf "friends_count" u +> JSON.as_int in
  let uid = JSON.getf "id" u +> json_as_int64 in
  (uid, fc)

let node_of_name acc name =
  let uid, _ = profile_of_id acc name in
  uid

module S = Set.Make (struct
  type t = node
  let compare = String.compare
end)
let list_of_s set =
  S.fold (fun x xs -> x::xs) set []

(*let mergex x xs =
  let rec iter store = function
    | [] -> store
    | x::xs -> iter
	  (if List.mem x store then store else x :: store) xs
  in
  iter l2 l1*)
let mergex x xs =
(*  x :: List.filter (fun y -> x<>y) xs*)
(*  if List.mem x xs then xs
  else x :: xs*)
  S.add x xs

let count = ref 0

let friends m node : node list =
  let rec loop total cursor =
    puts (!%"friendsloop:total:%d" !count);
    incr count;
    let cmd = !%"friends/ids/%s.json?cursor=%s" node cursor in
    let r = twitter (m.id,m.pass) cmd in
    let total' =
      List.fold_left (fun store j -> mergex (json_as_int64 j) store)
	total
	(JSON.getf "ids" r +> JSON.as_list)
    in
    let next_cursor =
      JSON.getf "next_cursor_str" r +> JSON.as_string
    in
    if next_cursor = "0" then list_of_s total'
    else loop (total') (next_cursor)
  in
  loop S.empty "-1"



let parse_input () =
  prerr_string "your id: @"; flush stderr;
  let twid = read_line() in
  prerr_string "password: "; flush stderr;
  let ps = read_line() in
  prerr_string "start: @"; flush stderr;
  let st_name = read_line() in
  prerr_string "target: @"; flush stderr;
  let gl_name = read_line() in
  prerr_endline "";
  puts (!%"input: @%s ->...-> @%s" st_name gl_name);
  let gl = node_of_name (twid,ps) gl_name in
  let st = node_of_name (twid,ps) st_name in 
  { id=twid; pass=ps; start=st; goal=gl }
    
let m = parse_input ()
let start : node
    = m.start
let goal : node
    = m.goal


let next : node -> node list 
    = fun node ->
      let fs = friends m node in
      puts (!%"next(%s) = [%s]" node (slist "," snode fs));
      fs

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

let validator acc node nextnodes =
  let _, fc = profile_of_id acc node in
  let r = (fc = List.length nextnodes) in
  if not r then puts (!%"validatorFalse!:%s" (snode node));
  r
    
let next = nmemoise next "tbl01"
    (fun key -> key)
    (fun ss -> !%"%d %s" (List.length ss) @@ slist " " snode ss)
    (fun s ->
      match Str.split delim s with
      | n :: ss -> List.map pnode ss
      | [] -> failwith "mustnothappen")
    (validator (m.id,m.pass))

let next n =
  try
    memoise next n
  with
  | e -> print_endline ("ERR:" ^ Printexc.to_string e);
      []

