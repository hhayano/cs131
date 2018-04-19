(* test convert_grammar *)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

type my_test_nonterminals =
  A | B | C | D | E | F

let my_test_rules = 
  [A, [T"a"];
  A, [N C; N B];
  B, [N A];
  C, [N D];
  E, [N F; N A];
  E, [N D];
  F, [N E]]

let my_test_grammar = A, my_test_rules
let converted = convert_grammar my_test_grammar

let a_list = 
  match converted with start_symbol, production_function ->
    production_function A
let b_list = 
  match converted with start_symbol, production_function ->
    production_function B
let c_list = 
  match converted with start_symbol, production_function ->
    production_function C
let d_list = 
  match converted with start_symbol, production_function ->
    production_function D
let e_list = 
  match converted with start_symbol, production_function ->
    production_function E 
let f_list = 
  match converted with start_symbol, production_function ->
    production_function F
