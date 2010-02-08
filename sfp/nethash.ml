open Util
open Putget
open JSON

type ('key, 'value) t = {
    tbl_id: string;
    key_of_json: (JSON.t -> 'key);
    json_of_key: ('key -> JSON.t);
    value_of_json: (JSON.t -> 'value);
    json_of_value: ('value -> JSON.t);
  }

let create table_id =
  fun (k_of_json, json_of_k)
      (v_of_json, json_of_v) ->
	{ tbl_id = table_id;
	  json_of_key = json_of_k;
	  key_of_json = k_of_json;
	  json_of_value = json_of_v;
	  value_of_json = v_of_json;
	}

(*let put_data, get_data =
  let data = ref "[]" in
  (fun tblid d -> data := d),
  (fun tblid -> !data)
  *)

let get_data t =
  match Putget.get t.tbl_id with
  | "" -> JSON.Array[]
  | s -> JSON.parse s
  
let get t k =
  try
    get_data t
      +> JSON.as_list
      +> List.map (fun o -> t.key_of_json(getf "key" o), getf "value" o)
      +> List.assoc k
      +> t.value_of_json
      +> fun x -> Some x
  with
  | Not_found -> None

let add t k v =
  let j =
    get_data t
  in
  let fs = JSON.as_list j in
  let f = JSON.Object [("key", t.json_of_key k);("value", t.json_of_value v)] in
  JSON.Array (f :: fs)
    +> JSON.to_string
    +> Putget.put t.tbl_id

