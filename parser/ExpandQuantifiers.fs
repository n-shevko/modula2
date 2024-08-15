module bnf.ExpandQuantifiers


open System
open System.Collections.Generic
open Utils
open AstToSyntax
open Ast

let mutable free_var = 0


type Quantifiers = IDictionary<Exp, Item>


let expand_rule (quantifiers: Quantifiers) quantifier branch =
    quantifiers[branch]


let rec expand_branch (quantifiers: Quantifiers) branch =
    match branch with
    | exp::tail ->
        let expanded_tail = expand_branch quantifiers tail in
        match exp with
        | TermOrNonterm(item, name, quantifier) ->
            match quantifier with
            | "" -> item::expanded_tail
            | _ -> quantifiers[exp]::expanded_tail
        | Group(exprs, name, quantifier) ->
            quantifiers[exp]::expanded_tail
    | [] -> []


let rec create_definition (quantifiers: Quantifiers) left_side exp =
    let branch, quantifier =
        match exp with
        | Group(content = content; quantifier = quantifier) ->
            [for exp' in content ->
                match exp' with
                | TermOrNonterm(item, name, quantifier) ->
                    match quantifier with
                    | "" -> item
                    | _ -> quantifiers[exp']
                | _ -> quantifiers[exp']], quantifier
        | TermOrNonterm(item = item; quantifier = quantifier) ->
            [item], quantifier in
    match quantifier with
    | "+" -> [branch@[left_side]; branch]
    | "*" -> [branch@[left_side]; [Terminal("")]]
    | "?" -> [branch; [Terminal("")]]


let name_for_exp exp =
    match exp with
    | TermOrNonterm(item, name, quantifier) ->
        if name = "" then
            match item with
            | Nonterminal(v) ->
                match quantifier with
                | "+" -> $"{v}_plus"
                | "*" -> $"{v}_star"
                | "?" -> $"{v}_optional"
            | Terminal(_) -> ""
        else
            name


let rec flatten_name_to_group exp_ls =
    flatten [for exp in exp_ls ->
                match exp with
                | TermOrNonterm(item, name, quantifier) ->
                    if quantifier <> "" then
                        [(name_for_exp exp, exp)]
                    else
                        []
                | Group(exp_ls', name, quantifier) ->
                    let rest = flatten_name_to_group exp_ls' in
                    if quantifier <> "" then
                        (name, exp)::rest
                    else
                        rest]


let rec split_with_name_and_without exp_ls =
    match exp_ls with
    | (name, _) as h::tail ->
        let with_name, noname = split_with_name_and_without tail in
        if name = "" then
            with_name, h::noname
        else
            h::with_name, noname
    | [] -> [], []


let get_noname_groups ast =
    join "\n\n" [for left, right in ast do
                let tmp = flatten [for branch in right -> flatten_name_to_group branch] in
                let with_name, noname = split_with_name_and_without tmp in
                if noname <> [] then
                    let tmp = join "\n" [for _, exp in noname -> exp_to_syntax exp] in
                    let left = left_side_to_str left in
                    yield $"{tmp} \nin {left}"]


let get_names_for_quantifiers ast =
    dict (flatten [for _, right in ast ->
                    let tmp = flatten [for branch in right -> flatten_name_to_group branch] in
                    let with_name, _ = split_with_name_and_without tmp in
                    [for name, item in with_name -> item, Nonterminal(name)]])


let rec expand_quantifiers ast =
    let quantifiers = get_names_for_quantifiers ast in
    let quantifiers_definitions = [for item in quantifiers ->
        match item.Value with Nonterminal(v) -> UsualNonterm(v),
        create_definition quantifiers item.Value item.Key
    ] in
    let expanded = [for left, branches in ast ->
                        (left, [for branch in branches -> expand_branch quantifiers branch])] in
    quantifiers_definitions@expanded
