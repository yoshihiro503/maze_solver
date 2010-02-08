open Util

(*  wget "localhost/cgi-bin/hashtbl.cgi?tbl_id=%d" t.tbl_id*)

let get data_id =
  try
    open_in_with("putgetlocal/"^data_id)
      (fun ch -> slist "\n" id (read_all ch))
  with
  | Sys_error _ -> ""

let put id data =
  open_out_with ("putgetlocal/"^id)
    (fun ch ->
      output_string ch data)
