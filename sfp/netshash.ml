open Util
open Wget

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
  puts (!%"add_data:%s" k);
(*  Wget.wget_with (!%"http://net.sp.land.to/add.cgi?id=%s&body=%s" k (t.string_of_value v)) ignore*)
  Wget.wget_with (!%"http://net.sp.land.to/add.cgi")
    ~postdata:(!%"tbl_id=%s&id=%s&body=%s" t.tbl_id k (t.string_of_value v)) ignore

let get_data t k =
  puts (!%"get_data:%s" k);
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
    slist "\n" id @@ List.rev (iter [] false)
  in
  Wget.wget_with (!%"http://net.sp.land.to/get.cgi?tbl_id=%s&id=%s" t.tbl_id k) read

let comma = Str.regexp ","

let get t k =
  try
    begin match Str.split comma (get_data t k) with
    | [key; date; body] ->
	Some (date, t.value_of_string body)
    | [key; date] ->
	Some (date, t.value_of_string "")
    | _ -> None
    end
  with
  | e -> None

let add t k v =
  add_data t k v
