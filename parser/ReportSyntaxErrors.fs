module bnf.ReportSyntaxErrors


open Utils


let pretify_branch branch =
    join " " [for item in branch ->
                match item with
                | Nonterminal v -> v.ToLower()
                | Terminal v -> let escaped = v.Replace("\n", "\\n") in $"'{escaped}'"]


let rec print_and_highlight branch highlight_item item_idx =
    match branch with
    | h::tail ->
        let underline, line = print_and_highlight tail highlight_item (item_idx + 1) in
        let marker = if highlight_item = item_idx then "^" else " " in
        match h with
        | Nonterminal(v) ->
            let v = v.ToLower() in
            (replicate v.Length marker) + " " + underline, $"{v} {line}"
        | Terminal(v) ->
            let v = v.ToLower().Replace("\n", "\\n") in
            (replicate v.Length marker) + $"{marker}{marker} " + underline, $"'{v}' {line}"
    | [] -> "", ""


let pretify_equation (left_side: string) right_side highlight_branch highlight_item =
    let pretty_branches = join "\n" (flatten [
        for idx, branch in enumerate(right_side) ->
            if highlight_branch = idx then
                let underline, line = print_and_highlight branch highlight_item 0 in
                [$"| {line}"; $"  {underline}"]
            else
                let line = pretify_branch branch in
                [$"| {line}"]
    ]) in
    let left_side = left_side.ToLower() in
    $"{left_side} =\n{pretty_branches}"


let rec follow_along_branch' last_successful_nonterm item_idx path (memo: Memo) offset =
    match path with
    | Terminal(v)::tail -> follow_along_branch' last_successful_nonterm (item_idx + 1) tail memo (offset + v.Length)
    | Nonterminal(v)::tail ->
            let found, res = memo.TryGetValue((v, offset)) in
            if found then
                let failed, _, finish, _ = res in
                if failed then
                    last_successful_nonterm
                else
                    follow_along_branch' (v, item_idx, finish) (item_idx + 1) tail memo finish
            else
                last_successful_nonterm
    | [] -> last_successful_nonterm


let follow_along_branch (memo: Memo) branch nonterm parsed =
    let item_idx = index (Nonterminal(nonterm)) branch in
    let path = skip (item_idx + 1) branch in
    follow_along_branch' (nonterm, -1, -1) (item_idx + 1) path memo parsed


let message_for_failed_branch (grammar: GrammarAsHash) (memo: Memo) =
    let deltas = [for kv in memo do
                    let _, start = kv.Key in
                    let failed, _, finish, branch_id = kv.Value in
                    if not failed then yield finish - start, kv] in
    let max_delta = max [for delta, _ in deltas -> delta] in
    let (nonerm, start), (_, _, finish, (caller_nonterm, caller_branch_id)) = item 0 [for delta, kv in deltas do if delta = max_delta then yield kv.Key, kv.Value]
    let branch_to_follow = item caller_branch_id grammar[caller_nonterm] in
    let last_successful_nonterm, item_idx, parsed' = follow_along_branch memo branch_to_follow nonerm finish in
    pretify_equation caller_nonterm grammar[caller_nonterm] caller_branch_id (item_idx + 1)


let rec insert_marker offset (lines: string list) target_char prev_line line_num =
    match lines with
    | line::tail ->
        if target_char >= offset && target_char <= (offset + line.Length) then
            let spaces_before = replicate (target_char - offset) " " in
            let next_line =
                if tail = [] then
                    ""
                else
                    tail[0]
            in
            (join "\n" [prev_line; line; $"{spaces_before}^"; next_line]).Trim(), line_num
        else
            insert_marker (offset + line.Length + 1) tail target_char line (line_num + 1)
    | [] -> "", -1


let report_syntax_errors result (memo: Memo) (src: string) (grammar: GrammarAsHash) =
    let failed, _, parsed_chars, _ = result in
    if parsed_chars = src.Length then
        ""
    else
        let max_offset = max [for kv in memo -> let nonterm, offset = kv.Key in offset] in
        let message = message_for_failed_branch grammar memo in
        let src_with_error, line_number = insert_marker 0 (split "\n" src) max_offset "" 1 in
        let delimeter = replicate 20 "-" in
        $"""Syntax error
Line: {line_number}
{delimeter}
{src_with_error}
{delimeter}
Failed rule:
{message}"""


