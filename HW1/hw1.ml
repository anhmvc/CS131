(* 1. subset a b -- returns whether list a is a subset of list b. *)
let subset a b =
    List.for_all(fun x -> List.exists(fun y -> x=y) b) a

(* 2. equal_sets a b -- returns whether the represented sets a and b are equal.  *)
let equal_sets a b =
    (subset a b) && (subset b a)

(* 3. set_union a b -- returns a list representing a∪b. Add all elements in a that are not in b to b*)
let rec set_union a b =
    match a with
    [] -> b
    | _ -> 
        (* if head of a is NOT in b, add head of a to b, call recursion with new appended b list and tail of a *)
        if not(List.mem (List.hd a) b) then set_union (List.tl a) (List.cons(List.hd a) b)
        (* if head of a IS in b, call recursion with tail of a and b *)
        else set_union (List.tl a) b

(* 4. set_symdiff a b -- returns the symmetric difference of a and b, that is, the set of all members of a∪b that are not also members of a∩b. *)
let set_symdiff a b =
    List.filter(fun x -> not(List.exists(fun y -> x=y) b)) a 
    @ List.filter(fun x -> not(List.exists(fun y -> x=y) a)) b

(* 5. Russell's Paradox *)

(* It is not possible to write such function in Ocaml. Ocaml has a strict types system, and all lists in Ocaml must be homogeneous (i.e. containing elements of the same type). For instance, given a set (list) of elements of an arbitrary type X, if the set is a member of itself, then the set would contain element of type 'list of type X' in addition to 'type X'. This contradicts the rule in Ocaml where it requires all lists to be homogeneous. *)

(* 6. computed_fixed_point eq f x -- returns the computed fixed point for f w.r.t x *)
let rec computed_fixed_point eq f x = 
    if (eq (f x) x) then x
    else computed_fixed_point eq f (f x)

(* 7. filter_reachable g -- returns a copy of the grammar g with all unreachable rules removed *)
type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal

let is_nonterminal symbol =
    match symbol with
    | N nonterminal -> true
    | _ -> false

(* filter rules that have the specified start *)
let filter_ruleset start rules =
    List.filter(fun rule -> (fst rule) = start) rules

let extract_symbol symbol = 
    match symbol with
    | N nonterminal -> nonterminal
    | _ -> failwith "Unable to extract symbol"

(* recursively find all reacheable RHS nonterminals from input ruleset *)
(* extract nonterminals i.e. N Expr -> Expr *)
let rec get_nonterminals_rhs rules results =
    match rules with
    [] -> results
    | _ -> 
        let rhs = snd (List.hd rules) in 
        let filtered_nonterms = List.map (fun x -> extract_symbol x) (List.filter(fun symbol -> is_nonterminal symbol) rhs) in
        let rest = (List.tl rules) in
        get_nonterminals_rhs rest (set_union results filtered_nonterms)

(* recursively get all reachable symbols from given symbols and ruleset *)
let rec get_reachable_symbols rules symbols results =
    match symbols with
    [] -> results
    | _ -> 
        let currStart = List.hd symbols in 
        let currRules = filter_ruleset currStart rules in
        let currNonterms = get_nonterminals_rhs currRules [] in
        let notVisited = List.filter(fun x -> not(subset [currStart] results)) currNonterms in 
        get_reachable_symbols rules (set_union notVisited (List.tl symbols)) (set_union [currStart] results)

let filter_reachable g =
    let start = fst g in
    let rules = snd g in
    let reachableRules = get_reachable_symbols rules [start] [] in
    start, List.filter(fun x -> subset [fst x] reachableRules) rules  