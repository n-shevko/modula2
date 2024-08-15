module bnf.EliminateLeftRecursion


open Utils
open Ast


let eliminate_left_recursion' left_side (right_side: (Item list) list) =
    match right_side with
    | (Nonterminal(v)::first_branch_tail)::rest_branches ->
        let new_nonterm = $"{v}_no_lrec" in
        let new_left_side =
            match left_side with
            | EntryPoint(v) -> EntryPoint(new_nonterm)
            | UsualNonterm(v) -> UsualNonterm(new_nonterm) in
        [(UsualNonterm(v), [Nonterminal(new_nonterm)::first_branch_tail; [Nonterminal(new_nonterm)]]);
         (new_left_side, rest_branches)]


let eliminate_left_recursion grammar =
    flatten [for left_side, right_side in grammar ->
                let left =
                    match left_side with
                    | EntryPoint(v) -> v
                    | UsualNonterm(v) -> v in
                match right_side with
                | (Nonterminal(v)::_)::_ ->
                    if left = v then // TODO: fix cases when left recursion not in the first branch
                        eliminate_left_recursion' left_side right_side
                    else
                        [left_side, right_side]
                | _ -> [left_side, right_side]]