open List

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
