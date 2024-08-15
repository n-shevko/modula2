module AstGenerator

open Utils
open System.IO
open System.Collections.Generic


let rec create_free_var used_vars pattern idx =
    if contains pattern used_vars then
        let var = $"{pattern}_{idx}" in
        if contains var used_vars then
            create_free_var used_vars pattern (idx + 1)
        else
            var
    else
        pattern


let rjust_table (table: (string list) list) =
    let table' = transpose table in
    let rjusted_cols = [for idx, column in enumerate(table') ->
                            let longest_value = max (map (fun (s: string) -> s.Length) column) in
                            [for v in column -> v.PadRight(longest_value, ' ')]] in
    transpose rjusted_cols


let rec type_for_branch items_in_branch used_vars postfix branch =
    if branch = [] then
        []
    else
        let star = if items_in_branch > 1 && branch.Length > 1 then "*" else " " in
        match branch with
        | Terminal(v)::tail ->
            let var = create_free_var used_vars "term" 2 in
            [var; $"  : string {star}"; $" // '{v}'"]::(type_for_branch items_in_branch (var::used_vars) postfix tail)
        | Nonterminal(v)::tail ->
            let var = create_free_var used_vars (v.ToLower()) 2 in
            [var; $"  : {v}{postfix} {star}"; ""]::(type_for_branch items_in_branch (var::used_vars) postfix tail)


let rec all_branches_have_only_one_terminal branches =
    match branches with
    | [Terminal(_)]::tail -> all_branches_have_only_one_terminal tail
    | [] -> true
    | _::tail -> false


let typedefs_for_original_ast' postfix left_side (branches: ('a list) list) =
    let last_branch_idx = branches.Length - 1 in
    let has_branch_with_multiple_args = (max [for branch in branches -> branch.Length]) > 1 in
    let spliter = if has_branch_with_multiple_args then "\n\n" else "\n" in
    join spliter [for branch_id, branch in enumerate(branches) ->
        let args = type_for_branch branch.Length [] postfix branch in
        let spliter = if branch_id = 0 then "  " else "| " in
        let args = [for idx, arg in enumerate(args) -> if idx = 0 then $"{spliter}{left_side}{branch_id}{postfix} of "::arg else ""::arg] in
        let rjusted = rjust_table args in
        join "\n" [for row in rjusted -> join "" row]
    ]


let typedefs_for_original_ast lang grammar =
    let grammar = capitalize_grammar grammar in
    let grammar_as_hash = grammar_to_hash (new GrammarAsHash()) grammar in
    let defs = join "\n\n\nand " [for nonterm, branches in grammar ->
        if all_branches_have_only_one_terminal branches then
            let comment = join " " [for branch in branches ->
                match branch with
                | [Terminal(v)] -> let v = v.Replace("\n", "\\n") in $"'{v}'"
            ] in
            $"{nonterm} = string // {comment}"
        else
            let args = typedefs_for_original_ast' "" nonterm branches in
            $"{nonterm} =\n{args}"
    ] in
    $"module {lang}.RawAst\n\n\ntype {defs}"


let create_ast entry_point folder grammar =
    let lang = last (split "/" folder) in
    let ast = typedefs_for_original_ast lang grammar in
    File.WriteAllText(Path.Combine(folder, "RawAst.fs"), ast)