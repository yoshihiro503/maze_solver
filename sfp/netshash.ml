open Util
open Wget

let limit = 5

type ('value) t = {
    tbl_id: string;
    string_of_value: ('value -> string);
    value_of_string: (string -> 'value);
  }

let create table_id =
  fun string_of_v v_of_string ->
	{ tbl_id = table_id;
	  string_of_value = string_of_v;
	  value_of_string = v_of_string;
	}

let add_data t k v =
  puts (!%"[%s]" (t.string_of_value v));
  let check ch =
    match maybe input_char ch with
    | `Val _ -> true
    | `Err _ -> false
  in
  Wget.wget_with (!%"http://net.sp.land.to/add.cgi")
    ~postdata:(!%"tbl_id=%s&id=%s&body=%s" t.tbl_id k (t.string_of_value v)) check

let rec add_data_repeet n t k v =
  if n > 0 then
    if add_data t k v = false then
      (puts"-repeet-"; Unix.sleep 3; add_data_repeet (n-1) t k v)
	

let get_data t k =
  puts (!%"get %s(%s)\n" t.tbl_id k);
  let read ch =
    let rec iter store start =
      try
	match start, input_line ch with
	| false, "BEGINCGI" -> iter store true
	| false, _ -> iter store false
	| true, "ENDCGI" -> store
	| true, l -> iter (l::store) true
      with
      | End_of_file -> store
    in
    List.rev (iter [] false)
  in
  Wget.wget_with (!%"http://net.sp.land.to/get.cgi?tbl_id=%s&id=%s" t.tbl_id k) read

let delete_data t k =
  puts (!%"DELETE%s:" k);
  let q = !%"http://net.sp.land.to/delete.cgi?tbl_id=%s&id=%s" t.tbl_id k in
  puts (!%"%s\n" q);
  Wget.wget_with q (fun ch -> puts @@ input_line ch);
  Unix.sleep 3
    
let comma = Str.regexp ","

let get t k =
  let take store line =
    try
      match Str.split comma line with
      | [key; date; body] ->
	  (date, t.value_of_string body) :: store
      | [key; date] ->
	  (date, t.value_of_string "") :: store
      | _ ->
	  puts (!%"get parse Err: line: %s\n" line);
	  store
    with
    | e -> store
  in
  List.fold_left take [] (get_data t k)

let add t k v = add_data_repeet limit t k v

let delete t k = delete_data t k


let memoise (f : 'a -> 'b) tblid skey serialize deserialize rvalidator =
  let tbl : ('b) t =
    create tblid serialize deserialize
  in
  fun x ->
    match get tbl (skey x) with
    | (_, y)::_ -> puts"(found)"; y
    | [] ->
	puts "(not found)";
	let y = f x in
	if rvalidator x y then add tbl (skey x) y;
	y


(*
let nmemoise (f : 'a -> 'b) tblid skey serialize deserialize rvalidator oflist each =
  let tbl =
    create tblid serialize deserialize
  in
  fun x ->
    match get tbl (skey x) with
    | dys ->
	let ys = List.map (fun (_,y) -> y) dys +> oflist in
	if rvalidator x ys then ys
	else begin
	  let ys = f x in
	  delete tbl (skey x);
	  each (fun y -> add tbl (skey x) y) ys;
	  ys
	end
*)

let nmemoise (f :'a -> 'b) tblid skey serialize deserialize xvalidator yvalidator ysvalid oflist each =
  let tbl =
    create tblid serialize deserialize
  in
  fun x ->
    match get tbl (skey x) with
    | dys ->
	let ys = List.map (fun (_,y) -> y) dys +> oflist in
	if xvalidator x ys then ys
	else begin
	  let newys = f x in
(*	  if isempty ys = false then delete tbl (skey x);*)
	  puts"ADDs:";
	  (if not @@ ysvalid ys newys then
	    each (fun y -> if yvalidator ys y then add tbl (skey x) y) newys);
	  puts"\n";
	  newys
	end

