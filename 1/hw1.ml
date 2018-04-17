open List

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

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

let rec delete_nongenerating rules_list generating =
  match rules_list with
  | [] -> []
  | hd::tl -> match hd with (lhs, rhs) ->
    if subset [lhs] generating = true then hd::(delete_nongenerating tl generating)
    else delete_nongenerating tl generating
;;

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
