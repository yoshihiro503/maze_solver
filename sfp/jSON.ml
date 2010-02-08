open Util
open ParserMonad

type t =
  | String of string
  | Number of float
  | Object of obj
  | Array of t list
  | Bool of bool
  | Null
and obj = (string * t) list

exception JSON_NotObject of t
exception JSON_InvalidField of (string)
exception JSON_CastErr of string

let show =
  let rec show_aux depth = function
    | String s -> "str(" ^s^ ")"
    | Number x -> !%"num(%f)" x
    | Object fs ->
	let indent d = String.make d '\t' in
	"{\n"
	^indent depth^ slist (",\n"^ indent depth) (fun (k,v) -> k^":"^ (show_aux (depth+1)) v) fs
	^"\n"^indent(depth-1)^"}"
    | Array xs -> "[" ^slist "," (show_aux depth) xs ^ "]"
    | Bool true -> "TRUE"
    | Bool false -> "FALSE"
    | Null -> "NULL"
  in
  show_aux 1

let to_string =
  let rec show_aux depth = function
    | String s -> !%"\"%s\"" s
    | Number x -> !%"%f" x
    | Object fs ->
	let indent = String.make depth '\t' in
	"{\n"^ indent^ slist (",\n"^ indent) (fun (k,v) ->
	  !%"\"%s\":%s" k (show_aux (depth+1) v)) fs^"}"
    | Array xs -> "[" ^slist "," (show_aux depth) xs ^ "]"
    | Bool true -> "true"
    | Bool false -> "false"
    | Null -> "null"
  in
  show_aux 1

let getf field t =
  match t with
  | Object o ->
      begin try List.assoc field o with
      | _ -> raise (JSON_InvalidField (field ^ " !<- [" ^ slist"," fst o ^ "]"))
      end
  | _ -> raise (JSON_NotObject t)
      
let as_bool = function
  | Bool true -> true
  | Bool false -> false
  | v -> raise (JSON_CastErr ("as_bool:" ^ show v))

let as_object = function
  | Object obj -> obj
  | v -> raise (JSON_CastErr ("as_object:" ^ show v))

let as_float = function
  | Number f -> f
  | v -> raise (JSON_CastErr ("as_float:" ^ show v))

let as_string = function
  | String s -> s
  | v -> raise (JSON_CastErr ("as_string:" ^ show v))

let as_list = function
  | Array l -> l
  | v -> raise (JSON_CastErr ("as_list:" ^ show v))

let as_int = function
  | Number f -> int_of_float f
  | v -> raise (JSON_CastErr ("as_int:" ^ show v))


(*parser*)

let whitespace = many (char '\n' <|> char ' ' <|> char '\t' <|> char '\r')

let string s =
  let rec iter i =
    if i < String.length s then
      char s.[i] >> iter (i+1)
    else return s
  in
  iter 0

let alp =
  char1 >>= fun c -> if c<>' ' && c<>'\n' && c<>'\t' && c<>'\r' then return c else error""

let alps0 = many alp
let alps = alp >>= fun c -> many alp >>= fun cs -> return (string_of_chars (c::cs))


type token =
  | ObjOpen
  | ObjClose
  | ListOpen
  | ListClose
  | Comma
  | Colon

  | TTrue
  | TFalse
  | TNull
  | TString of string
  | TNumber of float

let lit_string =
  let four_hex_digits =
    let hex = char1 >>= function
      | '0'..'9' | 'A'..'F' | 'a'..'f' as c -> return c
      | _ -> error ""
    in
    hex >>= fun d1 -> hex >>= fun d2 -> hex >>= fun d3 -> hex >>= fun d4 ->
      return (string_of_chars [d1;d2;d3;d4])
  in
  let lit_char =
    char1 >>= function
      | '\"' -> error ""
      | '\\' -> char1 >>=
	  begin function
	  | '\"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' as c -> return (string1 c)
	  | 'u' -> four_hex_digits
	  | _ -> error ""
	  end
      | c -> return (string1 c)
  in
  char '\"' >> many lit_char >>= fun ss -> char '\"' >> return (TString (slist "" id ss))

let digits =
  let digit =
    char1 >>= function
      | '0'..'9' | '-' | '.' | 'e' | 'E' | '+' as c -> return c
      | _ -> error "digit"
  in
  many1 digit >>= (return $ string_of_chars)

let lit_number = (* TODO *)
  digits >>= fun x -> return (TNumber (float_of_string x))

let token1 =
  let aux =
  (char '{' >> return ObjOpen)
    <|>
  (char '}' >> return ObjClose)
    <|>
  (char '[' >> return ListOpen)
    <|>
  (char ']' >> return ListClose)
    <|>
  (char ',' >> return Comma)
    <|>
  (char ':' >> return Colon)
    <|>
  (string "true" >> return TTrue)
    <|>
  (string "false" >> return TFalse)
    <|>
  (string "null" >> return TNull)
    <|>
  lit_string
    <|>
  lit_number
  in
  whitespace >> aux

let token t =
  token1 >>= fun x -> if t = x then return t else error "token"

let json_string =
  token1 >>= function TString s -> return s | _ -> error "json_string"

let json_number =
  token1 >>= function TNumber f -> return f | _ -> error "json_number"

let rec json (): t parser =
  begin
  let field =
    json_string >>= fun key -> token Colon >> json () >>= fun v -> return (key, v)
  in
  (token ObjOpen >> sep (token Comma) field >>= fun fields -> token ObjClose >>
    return @@ Object fields)
    <|>
  (token ListOpen >>= (fun _ -> sep (token Comma) (json()) >>= fun vs -> token ListClose >>
    return @@ Array vs))
    <|>
  (token TTrue >> return (Bool true))
    <|>
  (token TFalse >> return (Bool false))
    <|>
  (token TNull >> return Null)
    <|>
  (json_string >>= fun s -> return @@ String s)
    <|>
  (json_number >>= fun n -> return @@ Number n)
  end 
  

let parse_ch ch =
  try
    run_ch (json()) ch
  with
  | e -> print_endline (!%"JSON.parse err:[%s]" (Printexc.to_string e));
      raise e

let parse s =
  try
    run_string (json()) s
  with
  | e -> print_endline (!%"JSON.parse err:[%s]:\n%s" (Printexc.to_string e) s);
      raise e

