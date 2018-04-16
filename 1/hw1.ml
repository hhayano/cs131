open List
open Printf

let print_int_list l =
  iter (printf "%d ") l;
  printf "\n"
;;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

type test_nt = 
  | A | B | C | D | E | F

let print_test_nt nt =
  match nt with
  | A -> printf "N A, "
  | B -> printf "N B, "
  | C -> printf "N C, "
  | D -> printf "N D, "
  | E -> printf "N E, "
  | F -> printf "N F, "

let print_string_symbol sym =
  match sym with
  | N s -> print_test_nt s
  | T s -> printf "T %s, " s
;;

(*
let () = print_string_symbol (N A)
let () = print_string_symbol (T"x")
*)

let print_string_rule rule =
  match rule with (lhs, rhs) ->
    print_string_symbol lhs;
    printf " -> [";
    iter print_string_symbol rhs;
    printf "]\n"
;;

(*
let () = print_string_rule (N A, [T"a"; T"b"; T"c"])
*)

(* Subset *)
let rec subset s1 s2 =
  match s1 with
  | [] -> true
  | s::e -> match mem s s2 with
    | false -> false
    | true -> subset e s2
;;

(*
let () = printf "%B\n" (subset [] [1;2;3])
let () = printf "%B\n" (subset [3;1;3] [1;2;3])
let () = printf "%B\n" (not (subset [1;3;4] [1;2;3]))
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
let () = printf "%B\n" (equal_sets (set_union [] [1;2;3]) [1;2;3])
let () = printf "%B\n" (equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3])
let () = printf "%B\n" (equal_sets (set_union [] []) [])
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
let () = printf "%B\n" (equal_sets (set_intersection [] [1;2;3]) [])
let () = printf "%B\n" (equal_sets (set_intersection [3;1;3] [1;2;3]) [1;3])
let () = printf "%B\n" (equal_sets (set_intersection [1;2;3;4] [3;1;2;4]) [4;3;2;1])
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
let () = printf "%B\n" (equal_sets (set_diff [1;3] [1;4;3;1]) [])
let () = printf "%B\n" (equal_sets (set_diff [4;3;1;1;3] [1;3]) [4])
let () = printf "%B\n" (equal_sets (set_diff [4;3;1] []) [1;3;4])
let () = printf "%B\n" (equal_sets (set_diff [] [4;3;1]) [])
*)

(* computed fix point *)
let rec computed_fixed_point eq f x =
  match eq x (f x) with
  | true -> x
  | false -> computed_fixed_point eq f (f x)
;;

(*
let () = printf "%B\n" (computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0)
let () = printf "%B\n" (computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity)
let () = printf "%B\n" (computed_fixed_point (=) sqrt 10. = 1.)
let () = printf "%B\n" (((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
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
let () = printf "%B\n" (computed_periodic_point (=) (fun x -> x / 2) 0 (-1) = -1)
let () = printf "%B\n" (computed_periodic_point (=) (fun x -> x *. x -. 1.) 2 0.5 = -1.)
*)

(* while away *)
let rec while_away s p x =
  match p x with
  | false -> []
  | true -> x::(while_away s p (s x))
;;

(*
let () = printf "%B\n" (equal_sets (while_away ((+) 3) ((>) 10) 0) [0;3;6;9])
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
let () = printf "%B\n" (equal_sets (rle_decode [2,0; 1,6]) [0;0;6])
let () = printf "%B\n" (equal_sets (rle_decode [3,"w"; 1,"x"; 0,"y"; 2,"z"]) ["w"; "w"; "w"; "x"; "z"; "z"])
*)

(*
let rec remove_empty_rhs rules_list =
  match rules_list with
  | [] -> []
  | hd::tl ->
    match hd with (lhs, rhs) ->
      match rhs with
      | [] -> remove_empty_rhs tl
      | _ -> hd::(remove_empty_rhs tl)
;;
*)

(*
let () = printf "%B\n" (equal_sets (remove_empty_rhs [A, [T"0"]; B, []]) [A, [T"0"]])
let () = printf "%B\n" (equal_sets (remove_empty_rhs []) [])
let () = printf "%B\n" (equal_sets (remove_empty_rhs [A, []; B, [T"0"]]) [B, [T"0"]])
*)

let rec remove_redundant_rules rules_list =
  match rules_list with
  | [] -> []
  | hd::tl ->
    match hd with (lhs, rhs) ->
      match rhs with
      | [] -> hd::(remove_redundant_rules tl)
      | hd1::tl1 -> match hd1 with
        | T _ -> hd::(remove_redundant_rules tl)
        | N x ->
          if x = lhs then
            match tl with
            | [] -> remove_redundant_rules tl
            | _ -> hd::(remove_redundant_rules tl)
          else
            hd::(remove_redundant_rules tl)
;;

(* let () = printf "%B\n" (equal_sets (remove_redundant_rules [A , []; A, [T"0"]; B, [N B]]) [A, []; A, [T"0"]]) *)

let rec reduce_rules rules_list =
  let rec possible_NT rules_list = (* return non-terminals that have rules associated with them *)
    match rules_list with
    | [] -> []
    | hd::tl -> 
      match hd with (lhs, rhs) ->
        set_union [N lhs] (possible_NT tl)
  in
  let valid_NT = possible_NT rules_list in
  let rec remove_invalid_rules rules_list =
    match rules_list with
    | [] -> []
    | hd::tl ->
      match hd with (lhs, rhs) ->
        (* remove all the terminal symbols from the rhs so that we can use set_diff
         * to see if the rhs contains any non-terminal symbols that have no rules
         * associated with them *)
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

(* let () = printf "%B\n" (equal_sets (reduce_rules [A, [N E]; A, [N B]; A, [N B; N F]; B, [N C]; C, [T"c"]]) [A, [N B]; B, [N C]; C, [T"c"]]) *)

let find_generating_rules rules_list =
  let rec first_gen rules_list =
    match rules_list with
    | [] -> []
    | hd::tl -> 
      match hd with (lhs, rhs) ->
        let rec check_rhs rhs = (*assume that rhs will be non-empty, checks rhs if there are terminals or not *)
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
      | [] -> accepted
      | hd::tl -> 
        match hd with (lhs, rhs) ->
          let rec check_rhs rhs =
            match rhs with
            | [] -> true
            | hd::tl -> 
              match hd with
              | T _ -> check_rhs tl
              | N value -> 
                match mem value accepted with
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

(*
let rules_list = [A, [T"a"]; B, [N A]; C, [N D]; E, [N F]; F, [N E]]
let expected_list = [A; B]
let () = printf "%B\n" (equal_sets (find_generating_rules rules_list) expected_list)
*)

let rec delete_nongenerating rules_list generating =
  match rules_list with
  | [] -> []
  | hd::tl -> match hd with (lhs, rhs) ->
    if subset [lhs] generating = true then hd::(delete_nongenerating tl generating)
    else delete_nongenerating tl generating
;;

(*
let rules_list = [A, [T"a"]; B, [N A]; C, [N D]; E, [N F]; F, [N E]]
let generating = find_generating_rules rules_list
let expected_list = [A, [T"a"]; B, [N A]]
let () = printf "%B\n" (equal_sets (delete_nongenerating rules_list generating) expected_list)
*)

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
;;

let rec iter_gen_wrapper rules_list accepted =
  match rules_list with
  | [] -> accepted
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
;;

let rec check_rhs rhs accepted =
  match rhs with
  | [] -> true
  | hd::tl -> 
    match hd with
    | T _ -> check_rhs tl accepted
    | N _ -> 
      match mem hd accepted with
      | true -> check_rhs tl accepted
      | false -> false
;;

(* filter blind alley rules *)
let filter_blind_alleys g =
  match g with start_symbol, rules_list ->
    (* let no_empty = remove_empty_rhs rules_list in *)
    let no_redundant = remove_redundant_rules rules_list in
    let reduced = reduce_rules no_redundant in
    let rec filter rules_list =
      let generating = find_generating_rules rules_list in
      let only_generating_rules = delete_nongenerating rules_list generating in
      reduce_rules only_generating_rules
    in
    let reduced_rules_list = computed_fixed_point (equal_sets) filter reduced in
    start_symbol, reduced_rules_list
;;

(*
let rules_list = 
  [A, [T"a"];
  B, [N A];
  C, [N D];
  E, [N F];
  F, [N E]]
let expected_list = [A, [T"a"]; B, [N A]]
let grammar = A, rules_list
let () =
  let actual_list = 
    match filter_blind_alleys grammar with start_symbol, l ->
      l
  in
  printf "%B\n" (equal_sets actual_list expected_list)
*)


(*
let rules_list = N"A", [N"A", [N"A"]; N"A", []; N"A", [T"a"]; N"B", [N"A"]; N"C", [N"D"]; N"E", [N"F"]; N"F", [N"E"]]
let expected_list = [N"A", [T"a"]; N"B", [N"A"]]
(* let expected_list = [N"A"; N"B"] *)
let () = printf "%B\n" (equal_sets (filter_blind_alleys rules_list) expected_list)
let () = iter print_string_rule (filter_blind_alleys rules_list)
*)

(*
let () = printf "%B\n" (check_rhs [T"a"; T"b"; T"c"] [])
let () = printf "%B\n" (check_rhs [T"a"; T"b"; T"c"; N"A"] [])
let () = printf "%B\n" (check_rhs [T"a"; T"b"; T"c"; N"A"; T"d"] [N"A"])
let () = printf "%B\n" (check_rhs [T"a"; T"b"; T"c"; N"A"; T"d"] [])

let () = printf "%B\n" (equal_sets (first_gen [N"A", [T"a"]; N"B", [T"b"]; N"C", [T"c"]]) [N"A"; N"B"; N"C"])
let () = printf "%B\n" (equal_sets (first_gen [N"A", [T"a"]; N"B", [N"A"]; N"C", [T"c"]]) [N"A"; N"C"])
let l = find_generating_rules [N"A", [T"a"]; N"B", [T"b"]; N"C", [T"c"]]
let () = iter print_string_symbol l

let rules_list = [N"A", [T"a"]; N"B", [N"A"]; N"C", [N"c"]]
let () = printf "%B\n" (equal_sets (iter_gen_wrapper rules_list [N"A"]) [N"A"; N"B"])
let () = iter print_string_symbol (iter_gen_wrapper rules_list [N"A"])
let () = iter print_string_symbol (iter_gen_wrapper rules_list [])


let () = printf "%B\n" (equal_sets (find_generating_rules [N"A", [T"a"]; N"B", [N"A"]; N"C", [N"c"]]) [N"A"; N"B"])
let () = printf "%B\n" (equal_sets (delete_nongenerating [N"A", [T"a"]; N"B", [N"A"]; N"C", [N"c"]] [N"A"; N"B"]) [N"A", [T"a"]; N"B", [N"A"]])
*)

(* TEST CASES *)

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules

let awksub_test0 =
  printf "%B\n" (filter_blind_alleys awksub_grammar = awksub_grammar)

let awksub_test1 =
  printf "%B\n" (filter_blind_alleys (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules))

let awksub_test2 =
  printf "%B\n" (
  filter_blind_alleys (Expr,
      [Expr, [N Num];
       Expr, [N Lvalue];
       Expr, [N Expr; N Lvalue];
       Expr, [N Lvalue; N Expr];
       Expr, [N Expr; N Binop; N Expr];
       Lvalue, [N Lvalue; N Expr];
       Lvalue, [N Expr; N Lvalue];
       Lvalue, [N Incrop; N Lvalue];
       Lvalue, [N Lvalue; N Incrop];
       Incrop, [T"++"]; Incrop, [T"--"];
       Binop, [T"+"]; Binop, [T"-"];
       Num, [T"0"]; Num, [T"1"]; Num, [T"2"]; Num, [T"3"]; Num, [T"4"];
       Num, [T"5"]; Num, [T"6"]; Num, [T"7"]; Num, [T"8"]; Num, [T"9"]])
  = (Expr,
     [Expr, [N Num];
      Expr, [N Expr; N Binop; N Expr];
      Incrop, [T"++"]; Incrop, [T"--"];
      Binop, [T "+"]; Binop, [T "-"];
      Num, [T "0"]; Num, [T "1"]; Num, [T "2"]; Num, [T "3"]; Num, [T "4"];
      Num, [T "5"]; Num, [T "6"]; Num, [T "7"]; Num, [T "8"]; Num, [T "9"]])
  )

let awksub_test3 =
  printf "%B\n" (
  filter_blind_alleys (Expr, List.tl (List.tl (List.tl awksub_rules))) =
    filter_blind_alleys (Expr, List.tl (List.tl awksub_rules))
  )

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

let print_giant_nonterminals nt =
  match nt with
  | Conversation -> printf "N Conversation, "
  | Sentence -> printf "N Sentence, "
  | Grunt -> printf "N Grunt, "
  | Snore -> printf "N Snore, "
  | Shout -> printf "N Shout, "
  | Quiet -> printf "N Quiet, "

let print_giant_symbol sym =
  match sym with
  | N s -> print_giant_nonterminals s
  | T s -> printf "T %s, " s
;;

let print_giant_rules rule =
  match rule with (lhs, rhs) ->
    print_giant_nonterminals lhs;
    printf " -> [";
    iter print_giant_symbol rhs;
    printf "]\n"
;;


let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]]

let giant_test0 =
  printf "%B\n" (
  filter_blind_alleys giant_grammar = giant_grammar
  )

let () =
  let g = filter_blind_alleys giant_grammar in
  match g with ss, rl ->
    iter print_giant_rules rl
;;

let giant_test1 =
  printf "%B\n" (
  filter_blind_alleys (Sentence, List.tl (snd giant_grammar)) =
    (Sentence,
     [Quiet, []; Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Quiet]; Sentence, [N Grunt]; Sentence, [N Shout]])
  )

let giant_test2 =
  printf "%B\n" (
  filter_blind_alleys (Sentence, List.tl (List.tl (snd giant_grammar))) =
    (Sentence,
     [Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Grunt]; Sentence, [N Shout]])
  )
