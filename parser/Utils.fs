module Utils

open System.Collections.Generic
open System
open System.Reflection
open Microsoft.FSharp.Reflection


type Memo = Dictionary<Tuple<string, int>, Tuple<bool, obj array, int, Tuple<string, int>>>


type Item = Terminal of string | Nonterminal of string


type GrammarAsHash = Dictionary<string, (Item list) list>


let enumerate = List.indexed


let capitalizeFirst (s: string) =
    if s = "" then s
    else s.[0].ToString().ToUpper() + s.Substring(1)


let rec capitalize_grammar grammar =
    match grammar with
    | (left_side, right_side)::tail ->
        let res = (capitalizeFirst(left_side), List.map (fun branch ->
            List.map (function item ->
                        match item with
                        | Nonterminal v -> Nonterminal (capitalizeFirst v)
                        | x -> x
                    ) branch
            ) right_side) in
        res::(capitalize_grammar tail)
    | [] -> []


let rec grammar_to_hash (grammar_hash: GrammarAsHash) grammar =
    match grammar with
    | (nonterm, branches)::t ->
        grammar_hash[nonterm] <- branches
        grammar_to_hash grammar_hash t
    | [] -> grammar_hash


let ljust (str: string) size filling =
    if str.Length >= size then str
    else str + String.replicate (size - str.Length) filling


let flatten listOfLists = List.collect id listOfLists


let join = String.concat


let map = List.map


let max = List.max


let max_by fn ls = List.maxBy fn ls


let zip = List.zip


let zip3 = List.zip3


let sum = List.sumBy int


let contains = List.contains


let intersect list1 list2 = List.exists (fun x -> List.contains x list2) list1


let ofSeq = List.ofSeq


let rec unique ls =
    match ls with
    | h::tail ->
        if contains h tail then
            unique tail
        else
            h::(unique tail)
    | [] -> []


let item = List.item


let replicate times string = String.replicate times string


let filter = List.filter


let split (delimeter: string) (str: string) = Array.toList (str.Split([|delimeter|], StringSplitOptions.None))


let minus ls1 ls2 = List.except ls2 ls1


let rec replace idx new_val ls =
    match ls with
    | head::tail ->
        if idx = 0 then
            new_val::tail
        else
            replace (idx - 1) new_val tail
    | [] -> []


let count ls = List.countBy (fun x -> x) ls


let toArray = List.toArray


let toList = Array.toList


let last = List.last


let index elem ls = List.findIndex (fun x -> x = elem) ls


let skip n ls = List.skip n ls


let transpose matrix = List.transpose matrix


let group_by = List.groupBy


let rev = List.rev


let get (dict: Dictionary<'a, 'b>) (key: 'a) (dfl: 'b) =
    let found, value = dict.TryGetValue(key) in
    if found then
        value
    else
        dfl


let iget (dict: IDictionary<'a, 'b>) (key: 'a) (dfl: 'b) =
    let found, value = dict.TryGetValue(key) in
    if found then
        value
    else
        dfl


let rec group_in_dict' (result: Dictionary<'a, 'b list>) (ls: ('a * 'b) list) =
    match ls with
    | (k, v)::tail ->
        result[k] <- v::(get result k [])
        group_in_dict' result tail
    | [] -> result


let group_in_dict (ls: Tuple<'a, 'b> list) =
    let mutable result = Dictionary<'a, 'b list>() in
    group_in_dict' result ls


let unzip = List.unzip