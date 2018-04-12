open List
open Printf

(* Subset *)
let rec subset s1 s2 =
  match s1 with
  [] -> true
  | s::e -> match mem s s2 with
  false -> false
  | true -> subset e s2
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

(* Set Intersection *)
let rec set_intersection a b =
  match a with
  [] -> []
    | s::e -> match (mem s b) with
    true -> s::(set_intersection e b)
    | false -> set_intersection e b
;;

(* Set difference *)
let rec set_diff a b =
  match a with
  [] -> []
  | s::e -> match mem s b with
  true -> set_diff e b
  | false -> s::(set_diff e b)
;;

(* computed fix point *)
let rec computed_fixed_point eq f x =


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
let () = print_bool (subset [] [1;2;3])
let () = print_bool (subset [3;1;3] [1;2;3])
let () = print_bool (not (subset [1;3;4] [1;2;3]))

let () = print_bool (equal_sets (set_union [] [1;2;3]) [1;2;3])
let () = print_bool (equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3])
let () = print_bool (equal_sets (set_union [] []) [])

let set_intersection_test0 =
  print_bool (equal_sets (set_intersection [] [1;2;3]) [])

let set_intersection_test1 =
  print_bool (equal_sets (set_intersection [3;1;3] [1;2;3]) [1;3])

let set_intersection_test2 =
  print_bool (equal_sets (set_intersection [1;2;3;4] [3;1;2;4]) [4;3;2;1])

let () = print_bool (equal_sets (set_diff [1;3] [1;4;3;1]) [])
let () = print_bool (equal_sets (set_diff [4;3;1;1;3] [1;3]) [4])
let () = print_bool (equal_sets (set_diff [4;3;1] []) [1;3;4])
let () = print_bool (equal_sets (set_diff [] [4;3;1]) [])
*)
