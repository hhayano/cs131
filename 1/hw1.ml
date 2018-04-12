open List
open Printf

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

(* Set Union *)
let rec set_union a b =
  match a with
  [] -> b
  | s::e -> match (mem s b) with
  true -> set_union e b
  | false -> set_union e (append b [s])
;;
(*
  if a = [] then b else
    let first = List.hd a in
    let rest = List.tl a in
    match mem first b with
    | true -> set_union rest b
    | false -> set_union rest (List.append b [first])
;;
*)

(* Set Intersection *)
let rec set_intersection a b =
  match a with
  [] -> []
    | s::e -> match (mem s b) with
    true -> s::(set_intersection e b)
    | false -> set_intersection e b
;;

let print_bool bl =
  match bl with
  true -> print_endline "true"
  | false -> print_endline "false"
;;

let print_list l =
  iter (printf "%d ") l;
  printf "\n"
;;

(*
let () = print_bool (equal_sets (set_union [] [1;2;3]) [1;2;3])
let () = print_bool (equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3])
let () = print_bool (equal_sets (set_union [] []) [])
*)

let set_intersection_test0 =
  print_bool (equal_sets (set_intersection [] [1;2;3]) []);

let set_intersection_test1 =
  print_bool (equal_sets (set_intersection [3;1;3] [1;2;3]) [1;3]);

let set_intersection_test2 =
  print_bool (equal_sets (set_intersection [1;2;3;4] [3;1;2;4]) [4;3;2;1])

 
