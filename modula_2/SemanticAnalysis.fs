module modula_2.SemanticAnalysis


open Utils
open Ast


let rec split_on_functions_and_procedures declarations =
    match declarations with
    | head::tail ->
        let fns, procs = split_on_functions_and_procedures tail in
        match head with
        | Procedure(return_type = "") ->
            fns, head::procs
        | _ ->
            head::fns, procs
    | [] -> [], []


let get_procedure_declarations ast =
    match ast with
    | Module(block = block) ->
        match block with
        | declarations, _ ->
            [for decl in declarations do
                match decl with
                | Procedure(_) -> yield decl
                | _ -> ()]


let has_true ls =
    List.exists (fun x -> x = true) ls


let rec has_return_val statement =
    match statement with
    | ReturnWithValue(_) -> true
    | If_then(then_branch = then_branch) ->
        has_true [for statement in then_branch -> has_return_val statement]
    | If_then_else(then_branch = then_branch; else_branch = else_branch) ->
        (has_true [for statement in then_branch -> has_return_val statement]) or (has_true [for statement in else_branch -> has_return_val statement])
    | While(body = body) ->
        has_true [for statement in body -> has_return_val statement]
    | _ -> false


let has_return_val' proc_or_fn =
    match proc_or_fn with
    | Procedure(body = (_, statements)) ->
        has_true [for statement in statements -> has_return_val statement]


let semantic_analysis ast =
    // DIAG(err_function_requires_return, Error, "Function requires RETURN with value")
    // DIAG(err_procedure_requires_empty_return, Error, "Procedure does not allow RETURN with value")
    let declarations = get_procedure_declarations ast in
    let procs, funcs = split_on_functions_and_procedures declarations in
    let fns_without_return_val = [for fn in funcs do if not (has_return_val' fn) then yield fn] in
    let procs_with_return_val = [for proc in procs do if has_return_val' proc then yield proc]
    fns_without_return_val, procs_with_return_val
