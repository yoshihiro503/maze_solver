open Util

(* WGET
let fd = Unix.socket PF_INET SOCK_STREAM 0 in
Unix.connect fd (ADDR_INET (Unix.inet_addr_of_string "twitter.com/hoghoge", 80));
let ch = in_channel_of_descr fd in
read_all ch
*)

let wget_with ?(user="") ?(password="") ?(postdata="") (url:string) (f: in_channel -> 'a) =
  let o_user = if user="" then "" else !%"--user=\"%s\"" user in
  let o_pass = if password="" then "" else !%"--password=\"%s\"" password in
  let o_post = if postdata="" then "" else !%"--post-data=\"%s\"" postdata in
  let query =
    !%"wget %s %s %s \'%s\' -O -" o_user o_pass o_post url
  in
  puts (!%"wget:%s" query);
  Unix.sleep 1;
  let ch = Unix.open_process_in query in
  try
    let r = f ch in
    close_in ch; r
  with
  | e -> close_in ch; raise e

let wget ?(user="") ?(password="") ?(postdata="") (url:string) =
  wget_with ~user:user ~password:password ~postdata:postdata url
    (fun ch -> slist "\n" id @@ read_all ch)

