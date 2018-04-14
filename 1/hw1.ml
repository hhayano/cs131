open List
open Printf

(* Subset *)
let rec subset s1 s2 =
  match s1 with
  | [] -> true
  | s::e -> match mem s s2 with
    | false -> false
    | true -> subset e s2
;;

(* Equal Sets *)
let equal_sets a b =
  (subset a b) && (subset b a)
;;

(* Set Union *)
let rec set_union a b =
  match a with
  | [] -> b
  | s::e -> match (mem s b) with
    | true -> set_union e b
    | false -> set_union e (append b [s])
;;

(* Set Intersection *)
let rec set_intersection a b =
  match a with
  | [] -> []
  | s::e -> match (mem s b) with
    | true -> s::(set_intersection e b)
    | false -> set_intersection e b
;;

(* Set difference *)
let rec set_diff a b =
  match a with
  | [] -> []
  | s::e -> match mem s b with
    | true -> set_diff e b
    | false -> s::(set_diff e b)
;;

(* computed fix point *)
let rec computed_fixed_point eq f x =
  match eq x (f x) with
  | true -> x
  | false -> computed_fixed_point eq f (f x)
;;

(* computed periodic point *)
let rec computed_periodic_point eq f p x =
  let rec repeat f p x =
    match p with
    | 0 -> x
    | 1 -> f x
    | a -> repeat f (a-1) (f x)
  in
  match eq x (repeat f p x) with
  | true -> x
  | false -> computed_periodic_point eq f p (f x)
;;

(* while away *)
let rec while_away s p x =
  match p x with
  | false -> []
  | true -> x::(while_away s p (s x))
;;

(* rle decode *)
let rec rle_decode lp =
  match lp with
  | [] -> []
  | (i, e)::t -> let rec repeat length character =
    match length with
    | 0 -> []
    | x -> character::repeat (x-1) character
  in
  repeat i e @ rle_decode t
;;

let print_bool bl =
  match bl with
  | true -> print_endline "true"
  | false -> print_endline "false"
;;

let print_list l =
  iter (printf "%d ") l;
  printf "\n"
;;

(*
let () = print_bool (

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

let () = print_bool (computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0)
let () = print_bool (computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity)
let () = print_bool (computed_fixed_point (=) sqrt 10. = 1.)
let () = print_bool (((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
			 (fun x -> x /. 2.)
			 10.)
   = 1.25))

let () = print_bool (computed_periodic_point (=) (fun x -> x / 2) 0 (-1) = -1)
let () = print_bool (computed_periodic_point (=) (fun x -> x *. x -. 1.) 2 0.5 = -1.)

let () = print_bool (equal_sets (while_away ((+) 3) ((>) 10) 0) [0;3;6;9])

let () = print_bool (equal_sets (rle_decode [2,0; 1,6]) [0;0;6])
let () = print_bool (equal_sets (rle_decode [3,"w"; 1,"x"; 0,"y"; 2,"z"]) ["w"; "w"; "w"; "x"; "z"; "z"])
*)
