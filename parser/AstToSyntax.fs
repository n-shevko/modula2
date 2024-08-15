module bnf.AstToSyntax

open Ast
open Utils


let left_side_to_str left_side =
    match left_side with
    | UsualNonterm(v) -> v
    | EntryPoint(v) -> $"{{{v}}}"


let ast_to_syntax expanded_ast =
    join "\n\n" [for left_side, right_size in expanded_ast ->
                        let left_side' = left_side_to_str left_side in
                        let right_side' = join " | " [for branch in right_size ->
                                                        join " " [for item in branch ->
                                                                    match item with
                                                                    | Terminal(v) -> $"'{v}'"
                                                                    | Nonterminal(v) -> v]] in
                        $"{left_side'} = {right_side'};"]


let rec exp_to_syntax exp =
    match exp with
    | TermOrNonterm(item, name, quantifier) ->
        let item =
            match item with
            | Nonterminal(v) -> v
            | Terminal(v) -> $"'{v}'" in
        $"{item}{quantifier}"
    | Group(exp_ls, name, quantifier) ->
        let tmp = join " " [for exp in exp_ls -> exp_to_syntax exp] in
        $"({tmp}){quantifier}"


