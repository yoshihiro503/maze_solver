open Util
open JSON
open Wget

type node = string
type t = { id: string; pass: string; start: node; goal: node}

let snode node = !%"%s" node

let pnode s = 
  s

let node_dec n1 n2 =
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


let twitter (twid,ps) cmd =
  let url = "twitter.com/" ^ cmd in
  prerr_endline "sleep:";
  Unix.sleep 25;
  JSON.parse (Wget.wget ~user:twid ~password:ps url)

let json_as_int64 : JSON.t -> string =
  Int64.to_string $ Int64.of_float $ JSON.as_float

let profile_of_id acc idorname = (*TODO*)
  let u =
    twitter acc (!%"users/show/%s.json" idorname)
  in
  let fc = JSON.getf "friends_count" u +> JSON.as_int in
  let uid = JSON.getf "id" u +> json_as_int64 in
  let name = JSON.getf "screen_name" u +> JSON.as_string in
  (name, uid, fc)

let node_of_name acc name =
  let _, uid, _ = profile_of_id acc name in
  uid

module S = Set.Make (struct
  type t = node
  let compare = String.compare
end)
let list_of_set set =
  S.fold (fun x xs -> x::xs) set []
let set_of_list xs =
  List.fold_left (fun set x -> S.add x set) S.empty xs
let set_length s =
  S.fold (fun _ n -> succ n) s 0

let mergex x xs =
  S.add x xs

let count = ref 0

let friends m node =
  let rec loop total cursor =
    puts (!%"friendsloop:total:%d\n" !count);
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
    if next_cursor = "0" then total'
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
  puts (!%"input: @%s ->...-> @%s\n" st_name gl_name);
  let gl = node_of_name (twid,ps) gl_name in
  let st = node_of_name (twid,ps) st_name in 
  { id=twid; pass=ps; start=st; goal=gl }
    
let m = parse_input ()
let start : node
    = m.start
let goal : node
    = m.goal

let shownode node =
  let (name,uid,_) = profile_of_id (m.id,m.pass) node in
  !%"[%s:%s]" name uid

let next
    = fun node ->
      let fs = friends m node in
      puts (!%"next(%s) = {%s}\n" node (slist "," snode @@ list_of_set fs));
      fs

(*let netmemoise (f : 'a -> 'b) tblid skey serialize deserialize rvalidator =
  let tbl : ('b) Netshash.t =
    Netshash.create tblid serialize deserialize
  in
  fun x ->
    match Netshash.get tbl (skey x) with
    | Some (_, y) when rvalidator x y -> y
    | Some (_, y) ->
    | None ->
	let y = f x in
	if rvalidator x y then Netshash.add tbl (skey x) y;
	y*)



let delim = Str.regexp " "

let xvalid acc node nextnodes =
  let name,_, fc = profile_of_id acc node in
  let n = set_length nextnodes in
  if fc<>n then puts (!%"Xvalid NotEq!:%s(%s) (%d <> %d)\n" name (snode node) fc n);
  (fc <= n)

let xvalid_fast acc node nextnodes =
  S.is_empty nextnodes = false

let yvalid store y =
  let r = S.mem y store in
(*  puts (!%"Yvalid:len=%d %s ismem?: %b" (set_length store) y r);*)
  not r

let ysvalid store ys =
  let r = S.subset ys store in
(*  if not r then puts (!%"YSvalidFalse!: notSubst\n");*)
  r

let next = Netshash.nmemoise next "tbl03"
    (fun key -> key)
    (fun s -> s)
    (fun s -> s)
    (xvalid_fast (m.id,m.pass))
    (yvalid)
    ysvalid
    set_of_list
    S.iter
    

let next = memoise next

let next x =
  try
    list_of_set (next x)
  with
  | e -> puts (!%"Err:[%s]" (Printexc.to_string e));
      []
