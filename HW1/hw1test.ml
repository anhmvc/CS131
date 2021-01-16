let my_subset_test0 = subset [] []
let my_subset_test1 = subset [] [1;2]
let my_subset_test2 = not (subset [1;2] [])
let my_subset_test3 = subset [1] [1;2;3]
let my_subset_test4 = subset [1;2] [1;2;3]
let my_subset_test5 = subset [1;2;3] [1;2;3]
let my_subset_test6 = subset [3;1;3] [1;2;3]
let my_subset_test7 = subset [1;2;4] [4;1;2]
let my_subset_test8 = not (subset [1;3;7] [4;1;3])
let my_subset_test9 = not (subset [4] [1;2;3])
let my_subset_test10 = not (subset [1] [])
let my_subset_test11 = not(subset [1;2;4] [1;2;3])
let my_subset_test12 = not(subset [1;2;4] [1])
let my_subset_test13 = not(subset [1;2;5] [4;1;2;0])
let my_subset_test14 = subset ["a"] ["a";"b";"c";"d"]
let my_subset_test15 = not(subset ["a"] ["d";"c";"b"])

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1;2;3] [3;2;1;3]
let my_equal_sets_test2 = equal_sets [1] [1;1;1;1;1]
let my_equal_sets_test3 = equal_sets [1;3] [3;1;3]
let my_equal_sets_test4 = equal_sets [1;2] [2;1;2;1]
let my_equal_sets_test5 = not (equal_sets [1] [3;1;3])
let my_equal_sets_test7 = not (equal_sets [1;3;4] [3;1;3])
let my_equal_sets_test8 = not (equal_sets [1] [1;2;3;4])
let my_equal_sets_test9 = not (equal_sets [1;2;3] [3;1;3])
let my_equal_sets_test10 = not (equal_sets [] [1;2;3;4])
let my_equal_sets_test11 = equal_sets ["a";"b";"d"] ["a";"b";"d";"a"]
let my_equal_sets_test11 = not (equal_sets ["b";"d"] ["a";"b";"d";"a"])

let my_set_union_test0 = equal_sets (set_union [] []) []
let my_set_union_test1 = equal_sets (set_union [1;2;3] [4;5;6]) [1;2;3;4;5;6]
let my_set_union_test2 = equal_sets (set_union [1;2;3] [1;2;3]) [1;2;3]
let my_set_union_test3 = equal_sets (set_union [1;2;3] []) [1;2;3]
let my_set_union_test4 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let my_set_union_test5 = equal_sets (set_union [1;2;3;2;1] [1;2;3]) [1;2;3]
let my_set_union_test6 = equal_sets (set_union [1;2;3;4;3] [3]) [1;2;3;4]
let my_set_union_test7 = equal_sets (set_union ["a";"b";"c"] ["d";"e";"f"]) ["a";"b";"c";"d";"e";"f"]
let my_set_union_test8 = equal_sets (set_union ["a";"b"] ["b";"c"]) ["a";"b";"c"]

let my_set_symdiff_test0 = equal_sets (set_symdiff [] []) []
let my_set_symdiff_test1 = equal_sets (set_symdiff [] [1;2;3]) [1;2;3]
let my_set_symdiff_test2 = equal_sets (set_symdiff [1;2;3] []) [1;2;3]  
let my_set_symdiff_test3 = equal_sets (set_symdiff [4;1;3] [1;2;3]) [2;4]
let my_set_symdiff_test4 = equal_sets (set_symdiff [1;2;3;4;5] [5;3;1;2;4]) []
let my_set_symdiff_test5 = equal_sets (set_symdiff [1;2;3] [1;2;3;1;2;3]) []
let my_set_symdiff_test6 = equal_sets (set_symdiff [1] [5;3;1;2;4]) [2;3;4;5]
let my_set_symdiff_test7 = equal_sets (set_symdiff [1;2;3] [4;5;6]) [1;2;3;4;5;6]
let my_set_symdiff_test8 = equal_sets (set_symdiff [1;2;3] [4;5;6;1]) [2;3;4;5;6]
let my_set_symdiff_test9 = equal_sets (set_symdiff ["a";"b";"c"] ["b"]) ["a";"c"]
let my_set_symdiff_test10 = equal_sets (set_symdiff ["a";"b";"c"] ["b";"d"]) ["a";"c";"d"]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> 10) 0 = 10
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x *. x) 2. = infinity
let my_computed_fixed_point_test2 = computed_fixed_point (=) (fun x -> x / 10) 100000 = 0
let my_computed_fixed_point_test3 = equal_sets (computed_fixed_point equal_sets (fun x -> set_union x [1;2]) [2;3]) [1;2;3]

type nonterminals = A | B | C | D 
let grammar_rules =
   [A, [N B; N C; N D];
    A, [T "1"; N B; T "2"];
    B, [N C; N D];
    B, [T "1"; T "2"];
    C, [N D];
    C, [T "1"];
    D, [T "1"]]

let my_grammar = A, grammar_rules

let my_filter_reachable_test0 = filter_reachable my_grammar = my_grammar
let my_filter_reachable_test1 = filter_reachable (A, List.tl grammar_rules) =
    (A,
     [A, [T "1"; N B; T "2"];
      B, [N C; N D];
      B, [T "1"; T "2"];
      C, [N D];
      C, [T "1"];
      D, [T "1"]])
let my_filter_reachable_test2 = filter_reachable (B, grammar_rules) = 
    (B,
     [B, [N C; N D];
      B, [T "1"; T "2"];
      C, [N D];
      C, [T "1"];
      D, [T "1"]])
let my_filter_reachable_test3 = filter_reachable (C, grammar_rules) = 
    (C,
     [C, [N D];
      C, [T "1"];
      D, [T "1"]])
let my_filter_reachable_test4 = filter_reachable (D, grammar_rules) = 
    (D,
     [D, [T "1"]])