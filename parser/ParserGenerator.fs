module bnf.ParserGenerator

open System.IO

open Ast
open RawAst
open Utils
open AstGenerator
open SimplifyAst
open ExpandQuantifiers
open EliminateLeftRecursion
open SemanticAnalysis
open AstToSyntax
open Grammar
open Parser


let create_parse_function entry_point folder grammar =
    let lang = last (split "/" folder) in
    let entry_point_capitalized = capitalizeFirst entry_point in
    let grammar_as_str = sprintf "%A" grammar in
    let src = $"""module {lang}.Parser

open {lang}.RawAst
open Utils
open bnf.Parser


let grammar = {grammar_as_str}


let parse' (result: (bool * obj array * int * (string * int)) * string) =
    let (failed, ast, _, _), syntax_error = result in
    if failed then
        printfn "%%s" syntax_error
        None
    else
        let ast = ast[0] :?> {entry_point_capitalized} in
        Some (ast)


let parse src measure_timedelta =
    parse' (parse "{entry_point_capitalized}" grammar src measure_timedelta)


let parse_file file measure_timedelta =
    parse' (parse_file file "{entry_point_capitalized}" grammar measure_timedelta)

"""
    in
    File.WriteAllText(Path.Combine(folder, "Parser.fs"), src)


let simplify_ast' ast =
    [for left_side, right_side in ast ->
        let left_side =
                match left_side with
                | EntryPoint(v) -> v
                | UsualNonterm(v) -> v in
        left_side, right_side]


let save_expanded_to_file folder file_name ast =
    let file = Path.Combine(folder, $"expanded_{file_name}") in
    File.WriteAllText(file, ast_to_syntax ast)


let create_parser folder file_name measure_timedelta =
    let file = Path.Combine(folder, file_name) in
    let src = System.IO.File.ReadAllText(file) in
    let (failed, ast, _, _), error = parse "Bnf" grammar src measure_timedelta in
    if error <> "" then
        printfn "%s" error
    else
        let raw_ast = ast[0] :?> Bnf in
        let ast = simplify_ast raw_ast in
        let noname_groups = get_noname_groups ast in
        if noname_groups = "" then
            let ast = eliminate_left_recursion (expand_quantifiers (simplify_ast raw_ast)) in
            let errors = semantic_analysis ast in
            if errors = "" then
                save_expanded_to_file folder file_name ast
                let entry_point = (item 0 (get_entry_points ast)).ToLower() in
                let ast' = simplify_ast' ast in
                create_ast entry_point folder ast'
                create_parse_function entry_point folder ast'
                printf "Success"
            else
                printf $"Semantic errors:\n{errors}"
        else
            printf $"Name the following groups:\n\n{noname_groups}"


// let src = System.IO.File.ReadAllText("/home/nikos/data/Compiler Engineering/llvm/chapters_1_2/ConsoleApp1/compiler_engineering/modula_2/grammar.txt") in
// let x = parse "Bnf" grammar src false
// let y = 1
// let folder = "/home/nikos/data/Compiler Engineering/llvm/chapters_1_2/ConsoleApp1/compiler_engineering/bnf"
// create_typedefs "bnf" folder grammar


