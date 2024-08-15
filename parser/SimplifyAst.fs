module bnf.SimplifyAst

open Utils
open Ast
open RawAst


let rec var_to_string' var =
    match var with
    | Var0(var_symbol, tail) -> var_symbol::(var_to_string' tail)
    | Var1(var_symbol) -> [var_symbol]


let var_to_string var =
    join "" (var_to_string' var)


let rec terminal_symbols_to_string terminal_symbols =
    match terminal_symbols with
    | Terminal_symbols0(head, tail) -> head::(terminal_symbols_to_string tail)
    | Terminal_symbols1(term) -> [term]


let terminal_to_string term =
    match term with
    | Term0(terminal_symbols = terminal_symbols) ->
        join "" (terminal_symbols_to_string terminal_symbols)


let simplify_group_name group_name =
    match group_name with
    | Group_name0(var = var) -> var_to_string var
    | Group_name1(_) -> ""


let rec simplify_expr expr =
    match expr with
    | Expr0(group = group; expr = expr) ->
        (simplify_group group)::(simplify_expr expr)
    | Expr1(group) ->
        [simplify_group group]

and simplify_group group =
    match group with
    | Group0(expr = expr; group_name = group_name; quantifier_or_nothing = quantifier) ->
        Group(simplify_expr expr, simplify_group_name group_name, quantifier)
    | Group1(var, quantifier) ->
        TermOrNonterm(Nonterminal(var_to_string var), "", quantifier)
    | Group2(term, group_name, quantifier) ->
        TermOrNonterm(Terminal(terminal_to_string term), simplify_group_name group_name, quantifier)


let rec simplify_branch branch =
    match branch with
    | Branch0(group = group; branch = tail) ->
      (simplify_group group)::(simplify_branch tail)
    | Branch1(group) ->
        [simplify_group group]


let rec simplify_branches branches =
    match branches with
    | Branches0(branch = branch; branches = tail) ->
        (simplify_branch branch)::(simplify_branches tail)
    | Branches1(branch) ->
        [simplify_branch branch]


let simplify_equation equation =
    match equation with
    | Equation0(left_side = left_side; branches = branches) ->
        let left_side =
            match left_side with
            | Left_side0(var) -> UsualNonterm(var_to_string var)
            | Left_side1(var = var) -> EntryPoint(var_to_string var) in
        (left_side, simplify_branches branches)


let rec simplify_ast ast =
    match ast with
    | Bnf0(equation = equation; bnf = tail) ->
        (simplify_equation equation)::(simplify_ast tail)
    | Bnf1(equation = equation) ->
        [simplify_equation equation]
