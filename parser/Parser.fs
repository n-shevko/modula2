module bnf.Parser


open System.Collections.Generic
open System.Diagnostics
open System.Reflection
open Microsoft.FSharp.Reflection

open ReportSyntaxErrors
open Utils
open bnf.Ast


type BranchConstructors = Dictionary<string, UnionCaseInfo>


type ConstructorsHash = Dictionary<string, BranchConstructors>


let src_eq_to_terminal (src: string) (term: string) (offset: int) =
    try
        src.Substring(offset, term.Length) = term
    with
    | _ -> false


let rec parse_branch nonterm (constructors: ConstructorsHash) grammar (memo: Memo) branch branch_idx (src: string) offset caller =
    match branch with
    | term_or_nonterm::tail ->
        match term_or_nonterm with
        | Terminal left ->
            if src_eq_to_terminal src left offset then
                let failed, right, offset_right, _ = parse_branch nonterm constructors grammar memo tail branch_idx src (offset + left.Length) caller in
                if failed then
                    true, [||], offset, caller
                else
                    false, Array.append [|left :> obj|] right, offset_right, caller
            else
                true, [||], offset, caller
        | Nonterminal v ->
            let failed_left, left, offset_left, _ = parse_nonterm constructors grammar memo v grammar[v] src offset 0 (nonterm, branch_idx) in
            if not failed_left then
                let failed_right, right, offset_right, _ = parse_branch nonterm constructors grammar memo tail branch_idx src offset_left (nonterm, branch_idx) in
                if not failed_right then
                    false, Array.append left right, offset_right, (nonterm, branch_idx)
                else
                    true, [||], offset, (nonterm, branch_idx)
            else
                true, [||], offset, (nonterm, branch_idx)
    | [] -> false, [||], offset, caller

and parse_nonterm (constructors: ConstructorsHash) (grammar: GrammarAsHash) (memo: Memo) nonterm branches src offset branch_idx caller =
    let found, cache = memo.TryGetValue((nonterm, offset)) in
    if found then
        cache
    else
        match branches with
        | branch::tail ->
            let failed, args, new_offset, _ = parse_branch nonterm constructors grammar memo branch branch_idx src offset caller in
            let res =
                if failed then
                    parse_nonterm constructors grammar memo nonterm tail src offset (branch_idx + 1) caller
                else
                    let found, nonterm_constructors = constructors.TryGetValue(nonterm) in
                    if found then
                        let constructor = nonterm_constructors[$"{nonterm}{branch_idx}"] in
                        (false, [| FSharpValue.MakeUnion(constructor, args) :> obj|], new_offset, caller)
                    else
                        (false, args, new_offset, caller)
            in
            memo[(nonterm, offset)] <- res
            res
        | [] -> (true, [||], offset, caller)


let get_constructors_hash (folder: string) (mdl: string) =
    let folder = folder.ToLower() in
    let assembly : System.Reflection.Assembly = Assembly.GetExecutingAssembly() in
    let mutable constructors_hash = new ConstructorsHash() in
    for item in assembly.GetTypes() do
        if item.FullName.StartsWith($"{folder}.{mdl}") && item.DeclaringType <> null && item.DeclaringType.Name = mdl then
            let found, _ = constructors_hash.TryGetValue(item.Name) in
            if FSharpType.IsUnion(item) && not found && item.IsTypeDefinition then
                let mutable branches_hash = new BranchConstructors() in
                for branch in FSharpType.GetUnionCases(item) do
                    branches_hash[branch.Name] <- branch
                constructors_hash[item.Name] <- branches_hash
    constructors_hash


let parse root grammar src measure_timedelta =
    let grammar = capitalize_grammar grammar in
    let grammar_as_hash = grammar_to_hash (new GrammarAsHash()) grammar in
    let memo = new Memo() in
    let constructors = get_constructors_hash root "RawAst" in
    let result =
        if measure_timedelta then
            let stopwatch = Stopwatch.StartNew() in
            let result = parse_nonterm constructors grammar_as_hash memo root grammar_as_hash[root] src 0 0 (root, 0) in
            stopwatch.Stop()
            printfn "Parsing time: %d ms" stopwatch.ElapsedMilliseconds
            result, report_syntax_errors result memo src grammar_as_hash
        else
            let result = parse_nonterm constructors grammar_as_hash memo root grammar_as_hash[root] src 0 0 (root, 0) in
            result, report_syntax_errors result memo src grammar_as_hash
    in
    result


let parse_file file root grammar measure_timedelta =
    let src = System.IO.File.ReadAllText(file) in
    parse root grammar src measure_timedelta