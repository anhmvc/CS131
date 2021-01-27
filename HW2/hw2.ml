type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* PROBLEM 1 *)

(* An auxilary function that gets a list of rules that matches the same nonterminal and returns a list of the RHS of matching rules *)
let formPatterns rules nonterminal = 
    let filteredRules = List.split (List.filter(fun rule -> (fst rule) = nonterminal) rules) in
    (snd filteredRules)

(* A function that converts HW1-style grammar gram1 to HW2-style grammar *)
let convert_grammar gram1 =
    let start = fst gram1 in
    let rules = snd gram1 in
    (start, formPatterns rules) 

(* PROBLEM 2 *)
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* A function that traverses the parse tree left to right and yields a list of the leaves encountered *)
let parse_tree_leaves tree = 
    let rec aux leaves = function
        | [] -> leaves
        | head::tail -> match head with 
            | Leaf terminal -> aux (leaves@[terminal]) tail
            | Node (value, children) -> aux leaves (children@tail)            
    in aux [] [tree]


