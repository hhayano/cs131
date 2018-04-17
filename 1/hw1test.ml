(* Test cases *)

let my_subset_test0 = subset [] [1;2;3]
let my_subset_test1 = subset [3;1;1] [1;2;3]
let my_subset_test2 = not (subset [3;1] [])
let my_subset_test3 = not (subset [3;1] [1;2])

let my_equal_sets_test0 = equal_sets [1;3] [3;1]
let my_equal_sets_test1 = not (equal_sets [1;3] [3;1;2])
let my_equal_sets_test2 = equal_sets [1;3] [3;1;3]
let my_equal_sets_test3 = equal_sets [] []

let my_set_union_test0 = equal_sets (set_union [1;2] [1;3]) [1;2;3]
let my_set_union_test1 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [] []) []

let my_set_intersection_test0 = equal_sets (set_intersection [1;2] [1;3]) [1]
let my_set_intersection_test1 = equal_sets (set_intersection [2] [1;3]) []
let my_set_intersection_test2 = equal_sets (set_intersection [1;2;1] [1;3]) [1]

let my_set_diff_test0 = equal_sets (set_diff [1;2;3;4] [3;1;3]) [2;4]
let my_set_diff_test1 = equal_sets (set_diff [4;2;3;3] [3;1;3;4]) [2]
let my_set_diff_test2 = equal_sets (set_diff [4;2;3;3] []) [4;3;2]
let my_set_diff_test3 = equal_sets (set_diff [2;3] [1;2;3;4]) []

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x *. x) 0.9 = 0.

let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> -1*x) 2 4 = 4

let my_while_away_test0 = equal_sets (while_away ((+) 3) ((>) 10) 0) [0;3;6;9]
let my_while_away_test1 = equal_sets (while_away ((-) 3) ((=) 10) 0) []

let my_rle_decode_test0 = equal_sets (rle_decode [2,0; 1,6]) [0;0;6]
let my_rle_decode_test1 = equal_sets (rle_decode [3,"w"; 1,"x"; 0,"y"; 2,"z"]) ["w"; "w"; "w"; "x"; "z"; "z"]
let my_rle_decode_test2 = equal_sets (rle_decode [0,0]) []

type my_test_nonterminals =
  A | B | C | D | E | F

let my_test_rules = 
  [A, [T"a"];
  B, [N A];
  C, [N D];
  E, [N F];
  F, [N E]]

let my_test_revised_rules =
  [A, [T"a"];
  B, [N A]]

let my_test_grammar = A, my_test_rules
let my_test_expected_grammar = A, my_test_revised_rules

let my_filter_blind_alleys_test0 = filter_blind_alleys my_test_grammar = my_test_expected_grammar

