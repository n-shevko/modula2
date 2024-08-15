module modula_2.Main

open System.IO
open bnf
open bnf.Grammar
open modula_2.Parser
open SemanticAnalysis
open AstGenerator
open bnf.RawAst
open ExpandQuantifiers
open EliminateLeftRecursion
open AstToSyntax
open ParserGenerator
open modula_2.SimplifyAst
open Cfg
open Ssa
open LlvmIr


let folder = "/home/nikos/data/compiler_engineering/llvm/ConsoleApp1/compiler_engineering/modula_2"
let src = Path.Combine(folder, "test.txt")
let file = "grammar.txt"

let parse' (result: (bool * obj array * int * (string * int)) * string) =
    let (failed, raw_ast, _, _), syntax_error = result in
    if syntax_error <> "" then
        printfn "%s" syntax_error
        None
    else
        let raw_ast = raw_ast[0] :?> Bnf in
        Some (raw_ast)

// let file2 = "/home/nikos/data/compiler_engineering/llvm/ConsoleApp1/compiler_engineering/calc/grammar.txt"
//  "a = b (('vv' m)? g)+ | aa 'ds';"
//
//
// let res = parse' (parse "Bnf" grammar "a = b (('vv' m)? g)+ | aa 'ds';" false) //
// let ast = match res with Some(ast) -> simplify_ast ast
// create_ast "bnf" "/home/nikos/data/compiler_engineering/llvm/ConsoleApp1/compiler_engineering/parser" grammar

// create_parser folder file false
// printfn "%A" ast
// match result with
// | Some(_, ast) -> semantic_analysis ast
// let y = 1

// let src2 = Path.Combine(folder, "ssa_tests.txt")
// let ast' = match parse_file src2 false with Some(ast) -> ast
// let ast = simplify_ast ast'
// let proc = (get_procedures ast)["fn9"]
// let ssa = proc_to_ssa proc
// printfn "%A" ssa
// let c = 3


let vv = 3

compile "/home/nikos/data/compiler_engineering/llvm/ConsoleApp1/compiler_engineering/modula_2/ssa_tests.txt"
