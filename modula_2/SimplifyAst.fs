module modula_2.SimplifyAst

open RawAst
open Ast
open Utils
open System


let simplify_symbol ast =
    match ast with
    | Symbol0(letter) -> letter
    | Symbol1(digit) -> digit
    | Symbol2(underscore) -> underscore


let rec simplify_symbols ast =
    match ast with
    | Symbol_plus0(symbol, tail) ->
        (simplify_symbol symbol) + (simplify_symbols tail)
    | Symbol_plus1(symbol) ->
        simplify_symbol symbol


let simplify_identifier ast =
    match ast with
    | Identifier0(symbols) -> simplify_symbols symbols


let rec simplify_identifiers_tail ast =
    match ast with
    | Identifiers_tail0(identifier = identifier; identifiers_tail = tail) ->
        (simplify_identifier identifier)::(simplify_identifiers_tail tail)
    | Identifiers_tail1(_) -> []


let simplify_ident_list ast =
    match ast with
    | Ident_list0(identifier = identifier; identifiers_tail = tail) ->
        (simplify_identifier identifier)::(simplify_identifiers_tail tail)


let simplify_import ast =
    match ast with
    | Import0(from_optional = from_optional; ident_list = ident_list) ->
        match from_optional with
        | From_optional0(identifier = identifier) ->
            From(simplify_identifier identifier, simplify_ident_list ident_list)
        | _ ->
            Import(simplify_ident_list ident_list)


let rec simplify_imports ast =
    match ast with
    | Imports0(import = import; imports = tail) ->
        (simplify_import import)::(simplify_imports tail)
    | Imports1(_) -> []


let rec simplify_qualident' ast =
    match ast with
    | Qualidents_tail0(identifier = identifier; qualidents_tail = tail) ->
        (simplify_identifier identifier) + "." + (simplify_qualident' tail)
    | _ -> ""


let simplify_qualident ast =
    match ast with
    | Qualident0(identifier = head; qualidents_tail = tail) ->
        (simplify_identifier head) + (simplify_qualident' tail)


let name_type_ls ident_list qualident =
    let type' = simplify_qualident qualident in
    [for var in simplify_ident_list ident_list -> (var, type')]


let rec simplify_variable_declarations ast =
    match ast with
    | Variable_declarations0(variable_declaration = head; variable_declarations = tail) ->
        match head with
        | Variable_declaration0(ident_list = vars; qualident = type') ->
            [for v, t in name_type_ls vars type' -> VarDecl(v, t)]@(simplify_variable_declarations tail)
    | _ -> []


let simplify_formal_parameter ast =
    match ast with
    | Formal_parameter0(ident_list = vars; qualident = type') ->
        name_type_ls vars type'


let rec simplify_formal_parameters_tail ast =
    match ast with
    | Formal_parameters_tail0(formal_parameter = head; formal_parameters_tail = tail) ->
        (simplify_formal_parameter head)@(simplify_formal_parameters_tail tail)
    | _ -> []


let simplify_optional_return_type ast =
    match ast with
    | Optional_return_type0(qualident = qualident) ->
        simplify_qualident qualident
    | _ -> ""


let simplify_formal_parameters ast =
    match ast with
    | Formal_parameters0(formal_parameter_list_optional = args; optional_return_type = optional_return_type) ->
        let args =
            match args with
            | Formal_parameter_list_optional0(formal_parameter_list) ->
                match formal_parameter_list with
                | Formal_parameter_list0(formal_parameter, formal_parameters_tail) ->
                    (simplify_formal_parameter formal_parameter)@(simplify_formal_parameters_tail formal_parameters_tail)
            | _ -> [] in
        args, simplify_optional_return_type optional_return_type


let rec simplify_digit ast =
    match ast with
    | Digit_plus0(digit, tail) ->
        digit + (simplify_digit tail)
    | Digit_plus1(digit) ->
        digit


let simplify_number ast =
    match ast with
    | Number0(digit) -> Int32.Parse(simplify_digit digit)



let rec simplify_qualidents_tail ast =
    match ast with
    | Qualidents_tail0(_, identifier, qualidents_tail) ->
        (simplify_identifier identifier)::(simplify_qualidents_tail qualidents_tail)
    | Qualidents_tail1(_) -> []

and simplify_exprs_tail ast =
    match ast with
    | Exprs_tail0(expr = head; exprs_tail = tail) ->
        (simplify_expr head)::(simplify_exprs_tail tail)
    | _ ->
        []

and simplify_exp_list ast =
    match ast with
    | Exp_list0(expr, exprs_tail) ->
        (simplify_expr expr)::(simplify_exprs_tail exprs_tail)

and simplify_fn_call ast =
    match ast with
    | Fn_call0(qualident = fn_name; exp_list_optional = exp_list_optional) ->
        let args =
            match exp_list_optional with
            | Exp_list_optional0(exp_list) ->
                simplify_exp_list exp_list
            | Exp_list_optional1(_) ->
                [] in
        FnCall'(simplify_qualident fn_name, args)

and simplify_var_or_array_elem ast =
    match ast with
    | Var_or_array_elem0(identifier = var; expr = index) ->
        ArrayElemIndex(simplify_identifier var, simplify_expr index)
    | Var_or_array_elem1(qualident) ->
        match qualident with
        | Qualident0(identifier, Qualidents_tail1(_)) ->
            Var(simplify_identifier identifier)
        | Qualident0(identifier, qualidents_tail) ->
            StructField(simplify_identifier identifier, item 0 (simplify_qualidents_tail qualidents_tail))

and simplify_expr_no_lrec ast =
    match ast with
    | Expr_no_lrec0(expr = expr) ->
        simplify_expr expr
    | Expr_no_lrec1(expr = expr) ->
        Unary("not", simplify_expr expr)
    | Expr_no_lrec2(fn_call) ->
        simplify_fn_call fn_call
    | Expr_no_lrec3(number) ->
        Number(simplify_number number)
    | Expr_no_lrec4(var_or_array_elem) ->
        simplify_var_or_array_elem var_or_array_elem
    | Expr_no_lrec5(exp_list = exp_list) ->
        ArrayValue(simplify_exp_list exp_list)

and simplify_expr ast =
    match ast with
    | Expr0(expr_no_lrec = left; operation = operation; expr = right) ->
        Binary(simplify_expr_no_lrec left, operation, simplify_expr right)
    | Expr1(expr_no_lrec) ->
        simplify_expr_no_lrec expr_no_lrec


let rec simplify_constant_declarations ast =
    match ast with
    | Constant_declarations0(constant_declaration = head; constant_declarations = tail) ->
        let head =
            match head with
            | Constant_declaration0(identifier = var; expr = expr) ->
                Const(simplify_identifier var, simplify_expr expr) in
        head::(simplify_constant_declarations tail)
    | Constant_declarations1(_) -> []

let rec simplify_statement_sequence ast =
    match ast with
    | Statement_sequence0(statements) -> simplify_statements statements

and simplify_statement ast =
    match ast with
    | Statement0(var_or_array_elem = left; expr = right) ->
        Assignment(simplify_var_or_array_elem left, simplify_expr right)
    | Statement1(if_statement) ->
        match if_statement with
        | If_statement0(expr = cond; statement_sequence = then_branch; else_branch = else_branch) ->
            let cond = simplify_expr cond in
            let then_branch = simplify_statement_sequence then_branch in
            match else_branch with
            | Else_branch0(statement_sequence = statement_sequence) ->
                If_then_else(cond, then_branch, simplify_statement_sequence statement_sequence)
            | Else_branch1(_) ->
                If_then(cond, then_branch)
    | Statement2(while_statement) ->
        match while_statement with
        | While_statement0(expr = cond; statement_sequence = body) ->
            While(simplify_expr cond, simplify_statement_sequence body)
    | Statement3(optional_expr = ret_val) ->
        match ret_val with
        | Optional_expr0(expr = expr) ->
            ReturnWithValue(simplify_expr expr)
        | _ ->
            Return
    | Statement4(fn_call) ->
        match simplify_fn_call fn_call with
        | FnCall'(name, args) -> FnCall(name, args)


and simplify_statements ast =
    match ast with
    | Statements0(statement = head; statements = tail) ->
        (simplify_statement head)::(simplify_statements tail)
    | Statements1(statement = head) ->
        [simplify_statement head]


let simplify_statements_in_block ast =
    match ast with
    | Statements_in_block0(statement_sequence = statement_sequence) ->
        match statement_sequence with
        | Statement_sequence0(statements) -> simplify_statements statements
    | _ -> []


let rec simplify_typedefs ast =
    match ast with
    | Typedefs0(identifier = left; type_def = right; typedefs = tail) ->
        let right =
            match right with
            | Type_def0(number = elems_num; identifier = values_type) ->
                ArrayDef(simplify_number elems_num, simplify_identifier values_type)
            | Type_def1(variable_declarations = variable_declarations) ->
                let variable_declarations = [for VarDecl(v, t) in simplify_variable_declarations variable_declarations -> v, t] in
                StructDef(Fields(variable_declarations)) in
        TypeDef(simplify_identifier left, right)::(simplify_typedefs tail)
    | _ -> []


let rec simplify_procedure_declaration ast =
    match ast with
    | Procedure_declaration0(identifier = proc_name; formal_parameters_optional = args; block = block) ->
        let args, ret_type =
            match args with
            | Formal_parameters_optional0(formal_parameters) ->
                simplify_formal_parameters formal_parameters
            | _ -> [], "" in
        Procedure(simplify_identifier proc_name, args, ret_type, simplify_block block)

and simplify_declaration ast =
    match ast with
    | Declaration0(constant_declarations = constant_declarations) ->
        simplify_constant_declarations constant_declarations
    | Declaration1(variable_declarations = variable_declarations) ->
        simplify_variable_declarations variable_declarations
    | Declaration2(procedure_declaration = procedure_declaration) ->
        [simplify_procedure_declaration procedure_declaration]
    | Declaration3(typedefs = typedefs) ->
        simplify_typedefs typedefs

and simplify_declarations ast =
    match ast with
    | Declarations0(declaration = declaration; declarations = tail) ->
        (simplify_declaration declaration)@(simplify_declarations tail)
    | Declarations1(_) -> []

and simplify_block ast =
    match ast with
    | Block0(declarations = declarations; statements_in_block = statements_in_block) ->
        let statements =
            match statements_in_block with
            | Statements_in_block0(statement_sequence = statement_sequence) ->
                simplify_statement_sequence statement_sequence
            | Statements_in_block1(_) ->
                [] in
        simplify_declarations declarations, statements


let simplify_ast ast =
    match ast with
    | Modula_20(identifier = identifier; imports = imports; block = block) ->
        Module(simplify_identifier identifier, simplify_imports imports, simplify_block block)
