module modula_2.Cfg

open System.IO
open System
open Microsoft.FSharp.Quotations
open Utils
open System.Collections.Generic
open Ast


type Predecessors = Dictionary<int, int list>


type Block<'cond> =
  Statements of block_id      : int *
                statements    : Statement list *
                next_block_id : int

| Cond of block_id         : int *
          cond             : 'cond *
          true_branch_idx  : int *
          false_branch_idx : int


type Cfg<'cond> = IDictionary<int, Tuple<int list, Block<'cond>>>


let mutable free_id = 0


let replace_minus_1 block_id blocks =
    [for block in blocks ->
        match block with
        | Statements(blockId, statements, -1) ->
            let last = last statements in
            let next_block_id =
                match last with
                | Return(_) | ReturnWithValue(_) -> -1
                | _ -> block_id in
            Statements(blockId, statements, next_block_id)
        | Cond(cond_id, cond, true_branch_idx, -1) ->
            Cond(cond_id, cond, true_branch_idx, block_id)
        | _ -> block]


let rec create_cfg block statements =
    match statements with
    | statement::tail ->
        match statement with
        | If_then(_) | If_then_else(_) | While(_) ->
            let first_block_id_of_tail, tail_blocks = create_cfg [] tail in
            let cond_id = free_id in
            free_id <- free_id + 1
            let head =
                match statement with
                | If_then(cond, then_branch) ->
                    let then_first_block_id, then_blocks = create_cfg [] then_branch in
                    Cond(cond_id, cond, then_first_block_id, first_block_id_of_tail)::(replace_minus_1 first_block_id_of_tail then_blocks)
                | If_then_else(cond, then_branch, else_branch) ->
                    let then_first_block_id, then_blocks = create_cfg [] then_branch in
                    let else_first_block_id, else_blocks = create_cfg [] else_branch in
                    Cond(cond_id, cond, then_first_block_id, else_first_block_id)::(replace_minus_1 first_block_id_of_tail (then_blocks@else_blocks))
                | While(cond, body) ->
                    let body_first_block_id, body_blocks = create_cfg [] body in
                    Cond(cond_id, cond, body_first_block_id, first_block_id_of_tail)::(replace_minus_1 cond_id body_blocks)
            if block = [] then
                cond_id, head@tail_blocks
            else
                free_id <- free_id + 1
                free_id - 1, Statements(free_id - 1, rev block, cond_id)::head@tail_blocks
        | _ -> create_cfg (statement::block) tail
    | [] ->
        if block = [] then
            -1, []
        else
            let block_id = free_id in
            free_id <- free_id + 1
            block_id, [Statements(block_id, rev block, -1)]


let cfg_to_hash predecessors cfg =
    dict [for block in cfg ->
                match block with
                | Statements(block_id = block_id) ->
                    block_id, (get predecessors block_id [], block)
                | Cond(block_id = block_id) ->
                    block_id, (get predecessors block_id [], block)]


let calc_predecessors cfg =
    let mutable predecessors = Predecessors() in
    for block in cfg do
        let src, destinations =
            match block with
            | Statements(block_id, _, next_block_id)                -> block_id, [next_block_id]
            | Cond(block_id, _, true_branch_idx, false_branch_idx)  -> block_id, [true_branch_idx; false_branch_idx]
        in
        for dst in destinations do
            predecessors[dst] <- src::(get predecessors dst [])
    predecessors

