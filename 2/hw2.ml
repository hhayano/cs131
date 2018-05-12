open List

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

type my_test_nonterminals =
  A | B | C | D 

(* convert grammar *)
(*
 * old grammar = start_symbol, list_of_rules
 * new grammar = start_symbol, production_function
 * production_function: nonterminal -> alternative_list
 * alternative_list = list of rhs
 *)

let convert_grammar = function
  start_symbol, rules_list ->
    let rec create_production_function rules_list nonterminal =
      match rules_list with
      | [] -> []
      | h::t -> match h with lhs, rhs ->
        if lhs = nonterminal then rhs::(create_production_function t nonterminal)
        else create_production_function t nonterminal
    in
    start_symbol, create_production_function rules_list
;;

(*
let parse_prefix grammar =
  let matcher grammar accepter fragment =
    *)

let rec derive_nonterminals production_function = function (* Starting list of NT's *)
  | [] -> []
  | h::t -> 
    match h with
    | T _ -> h::(derive_nonterminals production_function t)
    | N _ -> 
      let rules = production_function h in (* assume that there's one rule for NT's for now *)
      match rules with
      | [] -> h::(derive_nonterminals production_function t)
      | h1::t1 -> (derive_nonterminals production_function h1)@(derive_nonterminals production_function t)
      (*
    | [] -> h::(derive_nonterminals rules_list t)
    | h::t -> 
        *)
;;

(* test derive_nonterminals *)
let prod_fun = function
  | N A -> [[N B; N C]]
  | N B -> [[T"b"]]
  | N C -> [[N D]]
  | N D -> [[T"d"]]
;;

let nonterms = derive_nonterminals prod_fun [N A]

