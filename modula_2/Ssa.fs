module modula_2.Ssa

open System.IO
open System
open Microsoft.FSharp.Quotations
open Utils
open System.Collections.Generic
open Ast
open Cfg
open modula_2.Ast


let mutable free_id = 0


let get_procedures ast =
    match ast with
    | Module(block = block) ->
        let declarations, _ = block in
        [for decl in declarations do
             match decl with
             | Procedure(name = name) ->
                // if name = "main" then
                    yield decl
             | _ -> ()]
    | _ -> []


type NumeratedVars = Dictionary<Tuple<string, int>, Exp>


type Id2VarName = Dictionary<int, string>


let rec numerate_local_vars_in_exp numerated_vars block_id exp =
    let numerate_local_vars_in_exp' = numerate_local_vars_in_exp numerated_vars block_id in
    match exp with
    | Binary(left, op, right) ->
        Binary(numerate_local_vars_in_exp' left, op, numerate_local_vars_in_exp' right)
    | Unary(op, exp) ->
        Unary(op, numerate_local_vars_in_exp' exp)
    | FnCall'(name, args) ->
        FnCall'(name, [for arg in args -> numerate_local_vars_in_exp' arg])
    | Var(var) ->
        get numerated_vars (var, block_id) exp
    | Number(value) ->
        Number(value)
    | ArrayElemIndex(var, index_exp) ->
        let found, v = numerated_vars.TryGetValue((var, block_id)) in
        if found then
            Get(v, Index(index_exp))
        else
            exp
    | ArrayValue(vals) ->
        ArrayValue([for v in vals -> numerate_local_vars_in_exp' v])
    | Malloc(_) ->
        exp
    | StructField(var, field) ->
        let found, v = numerated_vars.TryGetValue((var, block_id)) in
        if found then
            Get(v, Field(field))
        else
            exp


// array0 = initial_array
// value1 = get(array0, index)
// value2 = value1 + 1
// array1 = update(array0, index, value2)
// array2 = update(array1, index, value2)



let malloc_needed (numerated_vars: NumeratedVars) name =
    (sum [for item in numerated_vars -> let name', _ = item.Key in if name' = name then 1 else 0]) = 0


let create_local block_id =
    let var = Local(free_id, block_id) in
    free_id <- free_id + 1
    var, free_id - 1


let numerate_local_variables_in_statement (id2var_name: Id2VarName) numerated_vars block_id statement =
    let numerate_local_vars_in_exp' = numerate_local_vars_in_exp numerated_vars block_id in
    match statement with
    | Assignment(left, exp) ->
        let numerated, id = create_local block_id in
        match left with
        | Var(name) ->
            let new_assignment = Assignment(
                numerated,
                numerate_local_vars_in_exp' exp
            ) in
            id2var_name[id] <- name
            numerated_vars[(name, block_id)] <- numerated // should be after new_assignment creation for cases like: a = 12; a = a + 3;
            new_assignment
        | ArrayElemIndex(name, index_exp) ->
            let new_assingment = Assignment(
                numerated,
                Update(
                    numerate_local_vars_in_exp' (Var(name)),  // local when var is function argument or created on a previous line with array := [ ... ]; Ohterwise malloc. Differentiate cases: "first write" from "global var"
                    Index(numerate_local_vars_in_exp' index_exp),
                    numerate_local_vars_in_exp' exp
                )
            ) in
            id2var_name[id] <- name
            numerated_vars[(name, block_id)] <- numerated
            new_assingment
        | StructField(name, field) ->
            let new_assingment = Assignment(
                numerated,
                Update(
                    numerate_local_vars_in_exp' (Var(name)),
                    Field(field),
                    numerate_local_vars_in_exp' exp
            )) in
            id2var_name[id] <- name
            numerated_vars[(name, block_id)] <- numerated
            new_assingment
    | FnCall(name, args) ->
        FnCall(name, [for arg in args -> numerate_local_vars_in_exp' arg])
    | ReturnWithValue(exp) ->
        ReturnWithValue(numerate_local_vars_in_exp' exp)
    | Return ->
        Return


let local_value_numbering cfg (numerated_vars: NumeratedVars) (id2var_name: Id2VarName) =
    [for block in cfg ->
        match block with
        | Statements(block_id, statements, next_block_id) ->
            let statements = [for statement in statements -> numerate_local_variables_in_statement id2var_name numerated_vars block_id statement] in
            Statements(block_id, statements, next_block_id)
        | Cond(block_id, cond, true_branch_idx, false_branch_idx) ->
            let numerated_cond = numerate_local_vars_in_exp numerated_vars block_id cond in
            Cond(block_id, numerated_cond, true_branch_idx, false_branch_idx)]


let create_new_or_use_existing (numerated_vars: NumeratedVars) var block_id src operands =
    let existing = [for item in numerated_vars do
                        match item.Value with
                        | Phi(_, src', operands') ->
                            if src = src' && operands = operands' then
                                yield item.Value
                        | _ -> ()] in
    let phi =
        if existing = [] then
            let phi = Phi(free_id, src, operands) in
            free_id <- free_id + 1
            numerated_vars[(var, block_id)] <- phi
            phi
        else
            item 0 existing in
    let found, _ = numerated_vars.TryGetValue((var, src)) in
    if not found then
        numerated_vars[(var, src)] <- phi
    phi


let merge_search_results block_id a b =
    match a, b with
    | NotFound, _ -> b
    | _, NotFound -> a
    | Local(id, num_src_block), Phi(phi_id, src_block, _) ->
        Phi(-1, block_id, [(id, num_src_block); (phi_id, src_block)])
    | Local(id, src_block), Local(id', src_block') ->
        Phi(-1, block_id, [(id, src_block); (id', src_block')])
    | Phi(phi_id, src_block, _), Local(id', src_block') ->
        Phi(-1, block_id, [phi_id, src_block; id', src_block'])


let rec visit_predecessors (cfg: Cfg<Exp>) predecessors visited (numerated_vars: NumeratedVars) var block_id =
    match predecessors with
    | predecessor::tail ->
        let found, res = numerated_vars.TryGetValue((var, predecessor)) in
        let rest = visit_predecessors cfg tail (predecessor::visited) numerated_vars var block_id in
        if found then
            merge_search_results block_id res rest
        else
            if contains predecessor visited then
                NotFound
            else
                let predecessors_of_predecessor, _ = cfg[predecessor] in
                let predecessors_of_predecessor_res = visit_predecessors cfg predecessors_of_predecessor (predecessor::visited) numerated_vars var predecessor in
                let predecessors_of_predecessor_res =
                    match predecessors_of_predecessor_res with
                    | Local(id, src) -> Local(id, predecessor) // do this only if visit_predecessors returns phi, not local
                    | _ -> predecessors_of_predecessor_res in
                merge_search_results block_id predecessors_of_predecessor_res rest
    | [] -> NotFound


let rec numerate_global_vars_in_exp (cfg: Cfg<Exp>) (numerated_vars: NumeratedVars) block_id exp =
    let numerate_global_vars_in_exp' = numerate_global_vars_in_exp cfg numerated_vars block_id in
    match exp with
    | Binary(left, op, right) ->
        Binary(numerate_global_vars_in_exp' left, op, numerate_global_vars_in_exp' right)
    | Unary(op, exp) ->
        Unary(op, numerate_global_vars_in_exp' exp)
    | FnCall'(name, args) ->
        FnCall'(name, [for arg in args -> numerate_global_vars_in_exp' arg])
    | Var(var) ->
        let predecessors, _ = cfg[block_id] in
        let phi_or_local = visit_predecessors cfg predecessors [] numerated_vars var block_id in
        match phi_or_local with
        | Phi(-1, src_block, operands) -> create_new_or_use_existing numerated_vars var block_id src_block operands
        | _ -> phi_or_local
    | Number(value) ->
        Number(value)
    | ArrayElemIndex(var, index_expr) ->
        Get(
            numerate_global_vars_in_exp' (Var(var)),
            Index(numerate_global_vars_in_exp' index_expr)
        )
    | Update(src, index, value) ->
        let index = match index with Index(index) -> Index(numerate_global_vars_in_exp' index) | _ -> index in
        Update(
            numerate_global_vars_in_exp' src,
            index,
            numerate_global_vars_in_exp' value
        )
    | StructField(var, field) ->
        Get(numerate_global_vars_in_exp' (Var(var)), Field(field))
    | _ ->
        exp


let numerate_global_variables_in_statement (cfg: Cfg<Exp>) (numerated_vars: NumeratedVars) block_id statement =
    let numerate_global_vars_in_exp' = numerate_global_vars_in_exp cfg numerated_vars block_id in
    match statement with
    | Assignment(left, exp) ->
        Assignment(left, numerate_global_vars_in_exp' exp)
    | FnCall(name, args) ->
        FnCall(name, [for arg in args -> numerate_global_vars_in_exp' arg])
    | ReturnWithValue(exp) ->
        ReturnWithValue(numerate_global_vars_in_exp' exp)
    | Return ->
        Return


let global_value_numbering (cfg: Cfg<Exp>) (numerated_vars: NumeratedVars) =
     [for item in cfg ->
         let _, block = item.Value in
         match block with
         | Statements(block_id, statements, next_block_id) ->
             let numerated_statements = [for statement in statements ->
                 numerate_global_variables_in_statement cfg numerated_vars block_id statement] in
             Statements(block_id, numerated_statements, next_block_id)
         | Cond(block_id, cond, true_branch_idx, false_branch_idx) ->
             let numerated_cond = numerate_global_vars_in_exp cfg numerated_vars block_id cond in
             Cond(block_id, numerated_cond, true_branch_idx, false_branch_idx)]


let phi_assignments phi_defs blockId =
    [for phi in get phi_defs blockId [] ->
            match phi with
            | Phi(id = id; src_block = src_block) ->
                    Assignment(Local(id, src_block), phi)]


let insert_phi_defs_in_cfg (numerated_vars: NumeratedVars) cfg =
    let phi_defs = Dictionary<int, Exp list>() in
    for item in numerated_vars do
        match item.Value with
        | Phi(src_block = src_block) ->
            let block_phis = get phi_defs src_block [] in
            phi_defs[src_block] <- item.Value::block_phis
        | _ -> ()
    [for block in cfg ->
        match block with
        | Cond(blockId, cond, trueBranchIdx, falseBranchIdx) ->
            Cond(blockId, (phi_assignments phi_defs blockId, cond), trueBranchIdx, falseBranchIdx)
        | Statements(blockId, statements, nextBlockId) ->
            Statements(blockId, (phi_assignments phi_defs blockId)@statements, nextBlockId)]


let get_phi_defs (numerated_vars: NumeratedVars) =
    let phi_defs = unique [for item in numerated_vars do
                                match item.Value with
                                | Phi(src_block = src_block) ->
                                    yield (src_block, item.Value)
                                | _ -> ()] in
    group_in_dict phi_defs


let unpack_proc (type_descriptions: IDictionary<string, TypeDefinition>) proc =
    match proc with
    | Procedure(body = body; args = args; return_type = return_type) ->
        let declarations, statements = body in
        let mallocs = [for declaration in declarations do
                match declaration with
                | VarDecl(var_name, type_name) ->
                    let def = type_descriptions[type_name] in
                    match def with
                    | ArrayDef(_) | StructDef(_) ->
                        Assignment(Var(var_name), Malloc(def))
                    | _ -> ()]
        args, return_type, mallocs@statements


let proc_to_ssa type_descriptions proc =
    let args, return_type, statements = unpack_proc type_descriptions proc in
    let numerated_vars = NumeratedVars() in
    let id2var_name = Id2VarName() in
    Cfg.free_id <- 0
    let first_block_id, cfg = create_cfg [] statements in
    free_id <- Cfg.free_id
    let fn_args = [for var, t in args ->
                        numerated_vars[(var, first_block_id)] <- Local(free_id, first_block_id)
                        id2var_name[free_id] <- var
                        free_id <- free_id + 1
                        free_id - 1, t] in
    let cfg_with_local_numerated = local_value_numbering cfg numerated_vars id2var_name in
    let predecessors = calc_predecessors cfg in
    let cfg_as_hash = cfg_to_hash predecessors cfg_with_local_numerated in
    let numerated_cfg = global_value_numbering cfg_as_hash numerated_vars in
    let fn_name = match proc with Procedure(name = name) -> name in
    fn_name, fn_args, return_type, numerated_cfg, numerated_vars, id2var_name
