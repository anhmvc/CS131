type my_nonterminals = 
    | Sentence | VerbPhrase | NounPhrase | Verb | Noun | Adjective | Adverb

let my_rules = function
    | Sentence ->
            [[N NounPhrase; N VerbPhrase; N NounPhrase];
             [N Noun; N Verb; N Noun];
             [N NounPhrase; N VerbPhrase];
             [N Noun; N Verb]]
    | VerbPhrase ->
            [[N Verb];
             [N Adverb; N Verb];
             [N Verb; N Adverb]]
    | NounPhrase ->
            [[N Noun];
             [N Adjective; N Noun]]
    | Verb ->
            [[T "sings"];
             [T "eat"]]
    | Noun -> 
            [[T "Jungkook"];
             [T "besties"];
             [T "sushi"]]
    | Adjective ->
            [[T "crunchy"]]
    | Adverb ->
            [[T "beautifully"];
             [T "passionately"]]

let my_grammar = Sentence, my_rules

(* PROBLEM 5 *)
let test_matcher_frag = ["Jungkook"; "sings"; "beautifully"]
let make_matcher_test = 
    ((make_matcher my_grammar accept_all test_matcher_frag) = Some ["beautifully"])

(* PROBLEM 6 *)
let test_parser_frag = ["besties"; "passionately"; "eat"; "crunchy"; "sushi"]
let make_parser_test = 
    match make_parser my_grammar test_parser_frag with
    | Some tree -> parse_tree_leaves tree = test_parser_frag
    | _ -> false

let make_parser_test1 = 
    ((make_parser my_grammar test_parser_frag) =
        Some (Node (Sentence,
                            [Node (NounPhrase, 
                                    [Node (Noun, [Leaf "besties"])]);
                             Node (VerbPhrase,
                                    [Node (Adverb, [Leaf "passionately"]); 
                                     Node (Verb, [Leaf "eat"])]);
                             Node (NounPhrase,
                                    [Node (Adjective, [Leaf "crunchy"]);
                                     Node (Noun, [Leaf "sushi"])])])))