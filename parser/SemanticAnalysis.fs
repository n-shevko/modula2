module bnf.SemanticAnalysis

open Ast
open Utils


let get_entry_points expanded_ast =
    [for left_side, _ in expanded_ast do
        match left_side with
        | EntryPoint(v) -> yield v
        | _ -> ()]


let get_not_defined_nonterms expanded_ast =
    let defined_nonterms = [for left_side, _ in expanded_ast ->
                                match left_side with
                                | EntryPoint(v) -> v
                                | UsualNonterm(v) -> v] in
    let all_nonterms = flatten [for _, right_side in expanded_ast ->
                                    flatten [for branch in right_side ->
                                                    [for item in branch do
                                                        match item with
                                                        | Nonterminal(v) -> yield v
                                                        | _ -> ()]]] in
    unique (minus all_nonterms defined_nonterms)


let semantic_analysis expanded_ast =
    let entry_points_num = (get_entry_points expanded_ast).Length in
    let not_defined_nonterms = get_not_defined_nonterms expanded_ast in
    let mutable errors = [] in
    if entry_points_num <> 1 then
        errors <- $"Expected 1 entry point. Found {entry_points_num}"::errors
    if not_defined_nonterms <> [] then
        let not_defined_nonterms = join " " not_defined_nonterms in
        errors <- $"Not defined nonterms: {not_defined_nonterms}"::errors
    join "\n" errors
