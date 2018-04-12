open List

(* Subset *)
let rec subset s1 s2 =
  if s1 = [] then true
  else 
  let first = List.hd s1 in
  match List.mem first s2 with
  | false -> false
  | true -> subset (List.tl s1) s2
;;

(* Equal Sets *)
let equal_sets a b =
  (subset a b) && (subset b a)
;;

let isIn a l =
  let isEqual c =
    a = c
  in
  List.exists isEqual l
;;

let () =
  let bl = equal_sets [1;3;4] [3;1;3] in
  match bl with
  | true -> print_endline "true"
  | false -> print_endline "false";;
