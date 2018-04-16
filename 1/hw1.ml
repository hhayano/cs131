open List
open Printf

let print_bool bl =
  match bl with
  | true -> print_endline "true"
  | false -> print_endline "false"
;;

let print_int_list l =
  iter (printf "%d ") l;
  printf "\n"
;;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

let print_string_symbol sym =
  match sym with
  | N s -> printf "N %s" s
  | T s -> printf "T %s" s
;;

let print_string_rule rule =
  match rule with (lhs, rhs) ->
    


(* Subset *)
let rec subset s1 s2 =
  match s1 with
  | [] -> true
  | s::e -> match mem s s2 with
    | false -> false
    | true -> subset e s2
;;

(*
let () = print_bool (subset [] [1;2;3])
let () = print_bool (subset [3;1;3] [1;2;3])
let () = print_bool (not (subset [1;3;4] [1;2;3]))
*)

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

(*
let () = print_bool (equal_sets (set_union [] [1;2;3]) [1;2;3])
let () = print_bool (equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3])
let () = print_bool (equal_sets (set_union [] []) [])
*)

(* Set Intersection *)
let rec set_intersection a b =
  match a with
  | [] -> []
  | s::e -> match (mem s b) with
    | true -> s::(set_intersection e b)
    | false -> set_intersection e b
;;

(*
let () = print_bool (equal_sets (set_intersection [] [1;2;3]) [])
let () = print_bool (equal_sets (set_intersection [3;1;3] [1;2;3]) [1;3])
let () = print_bool (equal_sets (set_intersection [1;2;3;4] [3;1;2;4]) [4;3;2;1])
*)

(* Set difference *)
let rec set_diff a b =
  match a with
  | [] -> []
  | s::e -> match mem s b with
    | true -> set_diff e b
    | false -> s::(set_diff e b)
;;

(*
let () = print_bool (equal_sets (set_diff [1;3] [1;4;3;1]) [])
let () = print_bool (equal_sets (set_diff [4;3;1;1;3] [1;3]) [4])
let () = print_bool (equal_sets (set_diff [4;3;1] []) [1;3;4])
let () = print_bool (equal_sets (set_diff [] [4;3;1]) [])
*)

(* computed fix point *)
let rec computed_fixed_point eq f x =
  match eq x (f x) with
  | true -> x
  | false -> computed_fixed_point eq f (f x)
;;

(*
let () = print_bool (computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0)
let () = print_bool (computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity)
let () = print_bool (computed_fixed_point (=) sqrt 10. = 1.)
let () = print_bool (((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
			 (fun x -> x /. 2.)
			 10.)
   = 1.25))
*)

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

(*
let () = print_bool (computed_periodic_point (=) (fun x -> x / 2) 0 (-1) = -1)
let () = print_bool (computed_periodic_point (=) (fun x -> x *. x -. 1.) 2 0.5 = -1.)
*)

(* while away *)
let rec while_away s p x =
  match p x with
  | false -> []
  | true -> x::(while_away s p (s x))
;;

(*
let () = print_bool (equal_sets (while_away ((+) 3) ((>) 10) 0) [0;3;6;9])
*)

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

(*
let () = print_bool (equal_sets (rle_decode [2,0; 1,6]) [0;0;6])
let () = print_bool (equal_sets (rle_decode [3,"w"; 1,"x"; 0,"y"; 2,"z"]) ["w"; "w"; "w"; "x"; "z"; "z"])
*)

(* filter blind alley rules *)
(*
let filter_blind_alleys g =
  let rec all_terminal rhs =
    match rhs with
    | [] -> true
    | hd::tl ->
      match hd with
      | N _ -> false
      | T _ -> all_terminal tl
  in
  match g with
  | (start_symbol, rule_list) -> 
    let rec function filter_rules_list =
      [] -> []
    match rule_list with
    | [] -> (start_symbol, [])
    | hd::tl -> 
      match hd with
      | (lhs, rhs) ->
        match all_terminal rhs with
        | true -> 

let rec reduction_check rule rule_list =
  match rule with (lhs, rhs) ->
    match rhs with
    | [] -> false
    | 


let rec function filter_rules_list =
  | [] -> []
  | hd::tl -> 
    match hd with (lhs, rhs) -> 
      match all_terminal rhs with
      | true -> hd::(filter_rules_list tl)
      | false ->
        match reduction_check tl with
        | true -> hd::(filter_rules_list tl)
        | false -> filter_rules_list
;;

let rec all_terminal rhs =
  match rhs with
  | [] -> true
  | hd::tl ->
    match hd with
    | N _ -> false
    | T _ -> all_terminal tl
;;
*)

let rec remove_empty_rhs rules_list =
  match rules_list with
  | [] -> []
  | hd::tl ->
    match hd with (lhs, rhs) ->
      match rhs with
      | [] -> remove_empty_rhs tl
      | _ -> hd::(remove_empty_rhs tl)
;;

(*
let () = print_bool (equal_sets (remove_empty_rhs [N"0", [T"0"]; N"1", []]) [N"0", [T"0"]])
let () = print_bool (equal_sets (remove_empty_rhs []) [])
let () = print_bool (equal_sets (remove_empty_rhs [N"1", []; N"0", [T"0"]]) [N"0", [T"0"]])
*)

let rec remove_redundant_rules rules_list =
  match rules_list with
  | [] -> []
  | hd::tl ->
    match hd with (lhs, rhs) ->
      match rhs with
      | [e] ->
        if e = lhs then remove_redundant_rules tl
        else hd::(remove_redundant_rules tl)
      | _ -> hd::(remove_redundant_rules tl)
;;

(*
let () = print_bool (equal_sets (remove_redundant_rules [N"1", []; N"0", [T"0"]; N"1", [N"1"]]) [N"1", []; N"0", [T"0"]])
*)

let rec reduce_rules rules_list =
  let rec possible_NT rules_list =
    match rules_list with
    | [] -> []
    | hd::tl -> 
      match hd with (lhs, rhs) ->
        set_union [lhs] (possible_NT tl)
  in
  let valid_NT = possible_NT rules_list in
  let rec remove_invalid_rules rules_list =
    match rules_list with
    | [] -> []
    | hd::tl ->
      match hd with (lhs, rhs) ->
        let rec remove_T l =
          match l with
          | [] -> []
          | hd::tl ->
            match hd with
            | N _ -> hd::(remove_T tl)
            | T _ -> remove_T tl
        in
        let cleaned_rhs = remove_T rhs in
        match set_diff cleaned_rhs valid_NT with
        | [] -> hd::(remove_invalid_rules tl)
        | _ -> remove_invalid_rules tl
  in
  remove_invalid_rules rules_list
;;

(*
let () = print_bool (equal_sets (reduce_rules [N"A", [N"Q"]; N"A", [N"B"]; N"A", [N"B"; N"W"]; N"B", [N"C"]; N"C", [T"c"]]) [N"A", [N"B"]; N"B", [N"C"]; N"C", [T"c"]])
*)

let find_generating_rules rules_list =
  let rec first_gen rules_list =
    match rules_list with
    | [] -> []
    | hd::tl -> 
      match hd with (lhs, rhs) ->
        let rec check_rhs rhs = (*assume that rhs will be non-empty *)
          match rhs with
          | [] -> true
          | hd::tl -> 
            match hd with
            | N _ -> false
            | T _ -> check_rhs tl
        in 
        if check_rhs rhs = true then set_union [lhs] (first_gen tl) 
        else first_gen tl 
  in
  let first_gen_terms = first_gen rules_list in
  let iter_gen accepted =
    let rec iter_gen_wrapper rules_list accepted =
      match rules_list with
      | [] -> []
      | hd::tl -> 
        match hd with (lhs, rhs) ->
          let rec check_rhs rhs =
            match rhs with
            | [] -> true
            | hd::tl -> 
              match hd with
              | T _ -> check_rhs tl
              | N _ -> 
                match mem hd accepted with
                | true -> check_rhs tl
                | false -> false
          in 
          if check_rhs rhs = true then iter_gen_wrapper tl (set_union [lhs] accepted)
          else iter_gen_wrapper tl accepted
    in
    iter_gen_wrapper rules_list accepted
  in
  computed_fixed_point (equal_sets) iter_gen first_gen_terms
;;

let () = print_bool (equal_sets (find_generating_rules [N"A", [T"a"]; N"B", [T"b"]; N"C", [T"c"]]) [N"A"; N"B"; N"C"])
let l = find_generating_rules [N"A", [T"a"]; N"B", [T"b"]; N"C", [T"c"]] in



(*
let () = print_bool (
*)

