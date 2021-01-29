type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* PROBLEM 1 *)

(* An auxilary function that gets a list of rules that matches the same nonterminal and returns a list of the RHS of matching rules *)
let form_patterns rules nonterminal = 
    let filtered_rules = List.split (List.filter(fun rule -> (fst rule) = nonterminal) rules) in
    (snd filtered_rules)

(* A function that converts HW1-style grammar gram1 to HW2-style grammar *)
let convert_grammar gram1 = match gram1 with
  | (start, rules) -> (start, form_patterns rules) 

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

(* PROBLEM 3 *)

(* PRE-DEFINED ACCEPTOR FUNCTIONS *)
let accept_all string = Some string
let accept_empty_suffix = function
  | _::_ -> None
  | x -> Some x

let rec match_rules grammar sub_list accept frag = 
  match sub_list with
  | [] -> None (* Rules list is empty, returns None *)
  | head::tail -> 
    let result = (match_current_rule grammar head accept frag) in
    match result with
    | None -> (match_rules grammar tail accept frag) (* no match, recursively call with the rest of the list *)
    | _ -> result 
(* A function that returns the result of acceptor when taking in a fragment with one rule *)
and match_current_rule grammar rule accept frag =
  match rule with
  | [] -> accept frag (* empty list, run acceptor with the current fragment *)
  | _ -> match frag with
        | [] -> None
        | frag_hd::frag_tl -> match rule with
              | [] -> None
              | rule_hd::rule_tl -> match rule_hd with
                    | T terminal -> if terminal = frag_hd then match_current_rule grammar rule_tl accept frag_tl
                                    else None
                    | N nonterminal -> 
                    let new_acceptor = match_current_rule grammar rule_tl accept in 
                    match_rules grammar (grammar nonterminal) new_acceptor frag

(* A function that returns a matcher for the grammar *)
let make_matcher gram = match gram with
  | (start, rules) -> fun accept frag -> match_rules rules (rules start) accept frag

(* PROBLEM 4 *)

(* An acceptor that checks if inputted fragment is a full match *)
let parser_acceptor frag tree = match frag with
  | [] -> Some tree
  | _ -> None

(* Minor modifications to make_matcher to include an accumulator path to build our parse tree *)
let rec parse_rules_tree grammar start_symbol sub_list accept frag path = 
  match sub_list with
  | [] -> None (* Rules list is empty, returns None *)
  | head::tail -> 
    let result = (parse_current_rule grammar start_symbol head accept frag path) in
    match result with
    (* No matches found, continue to the next rule on the same rule list with that nonterminal *)
    | None -> (parse_rules_tree grammar start_symbol tail accept frag path)
    (* found a match, continue to recursively call curr_rule function with the appended path and next symbol *)
    | _ -> result 
(* A function that returns the result of acceptor when taking in a fragment with one rule *)
and parse_current_rule grammar start_symbol rule accept frag path =
  (* create Node appending start symbol to current path *)
  let curr_node = (Node(start_symbol, path)) in
  match rule with
  | [] -> accept frag curr_node (* if there is no more rules, add current_node to parse tree*) 
  | _ -> match frag with
        | [] -> None
        | frag_hd::frag_tl -> match rule with
              | [] -> None
              | rule_hd::rule_tl -> 
                    match rule_hd with
                    | T terminal -> if terminal = frag_hd then 
                          let curr_leaf = (Leaf terminal) in
                          parse_current_rule grammar start_symbol rule_tl accept frag_tl (path@[curr_leaf]) 
                                    else None
                    | N nonterminal -> 
                    let new_acceptor new_frag new_path = parse_current_rule grammar start_symbol rule_tl accept new_frag (path@[new_path]) in 
                    parse_rules_tree grammar nonterminal (grammar nonterminal) new_acceptor frag []

(* A function that returns a parser  *)
let make_parser gram = match gram with
  | (start, rules) -> (fun frag ->  parse_rules_tree rules start (rules start) parser_acceptor frag [])