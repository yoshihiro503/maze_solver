let (@@) f x = f x
let ($) g f x = g (f x)

let si = string_of_int
let string1 = String.make 1
let slist delim f xs = String.concat delim @@ List.map f xs

let explode s = 
  let rec iter n =
    if n >= String.length s then
      []
    else 
      s.[n] :: iter (n+1)
  in 
  iter 0

let (>>=) o f =
  match o with
  | Some x -> f x
  | None -> None
let return x = Some x
let opt_map f o = o >>= (return $ f)
let is_some = function Some _ -> true | None -> false
let opt_flat os = List.concat @@ List.map (function Some x -> [x] | None -> []) os

let matrix_of_list xss = Array.of_list @@ List.map Array.of_list xss
let arr_find p a =
  let n = Array.length a in
  let rec iter i =
    if i < n then
      if p a.(i) then Some a.(i)
      else iter (i+1)
    else None
  in
  iter 0
let matrix_find p m =
  arr_find is_some @@ Array.map (arr_find p) m
let matrix_find_idx p m =
  let idx = ref (0,0) in
  Array.iteri (fun j -> Array.iteri (fun i x -> (*print_endline (si i^","^si j^": "^ string_of_bool (p x));*)
    if p x then idx := (i,j))) m;
  !idx
let sarr delim f a = slist delim f @@ Array.to_list a
let smatrix f m = sarr "\n" (sarr "" f) m
