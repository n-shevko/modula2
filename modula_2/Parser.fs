module modula_2.Parser

open modula_2.RawAst
open Utils
open bnf.Parser


let grammar = [("digit_plus",
  [[Nonterminal "digit"; Nonterminal "digit_plus"]; [Nonterminal "digit"]]);
 ("symbol_plus",
  [[Nonterminal "symbol"; Nonterminal "symbol_plus"]; [Nonterminal "symbol"]]);
 ("delimeter_star",
  [[Nonterminal "delimeter"; Nonterminal "delimeter_star"]; [Terminal ""]]);
 ("identifiers_tail",
  [[Nonterminal "delimeter_star"; Terminal ","; Nonterminal "delimeter_star";
    Nonterminal "identifier"; Nonterminal "identifiers_tail"]; [Terminal ""]]);
 ("qualidents_tail",
  [[Terminal "."; Nonterminal "identifier"; Nonterminal "qualidents_tail"];
   [Terminal ""]]);
 ("from_optional",
  [[Terminal "from"; Nonterminal "delimeter_plus"; Nonterminal "identifier";
    Nonterminal "delimeter_plus"]; [Terminal ""]]);
 ("delimeter_plus",
  [[Nonterminal "delimeter"; Nonterminal "delimeter_plus"];
   [Nonterminal "delimeter"]]);
 ("exprs_tail",
  [[Nonterminal "delimeter_star"; Terminal ","; Nonterminal "delimeter_star";
    Nonterminal "expr"; Nonterminal "exprs_tail"]; [Terminal ""]]);
 ("exp_list_optional", [[Nonterminal "exp_list"]; [Terminal ""]]);
 ("optional_var", [[Terminal "var"]; [Terminal ""]]);
 ("formal_parameters_tail",
  [[Nonterminal "delimeter_star"; Terminal ";"; Nonterminal "delimeter_star";
    Nonterminal "formal_parameter"; Nonterminal "formal_parameters_tail"];
   [Terminal ""]]);
 ("formal_parameter_list_optional",
  [[Nonterminal "formal_parameter_list"]; [Terminal ""]]);
 ("optional_return_type",
  [[Nonterminal "delimeter_star"; Terminal ":"; Nonterminal "delimeter_star";
    Nonterminal "qualident"]; [Terminal ""]]);
 ("constant_declarations",
  [[Nonterminal "delimeter_star"; Nonterminal "constant_declaration";
    Nonterminal "delimeter_star"; Terminal ";";
    Nonterminal "constant_declarations"]; [Terminal ""]]);
 ("variable_declarations",
  [[Nonterminal "delimeter_star"; Nonterminal "variable_declaration";
    Nonterminal "delimeter_star"; Terminal ";";
    Nonterminal "variable_declarations"]; [Terminal ""]]);
 ("typedefs",
  [[Nonterminal "identifier"; Nonterminal "delimeter_star"; Terminal "=";
    Nonterminal "delimeter_star"; Nonterminal "type_def";
    Nonterminal "delimeter_star"; Terminal ";"; Nonterminal "typedefs"];
   [Terminal ""]]);
 ("optional_expr",
  [[Nonterminal "delimeter_star"; Nonterminal "expr"]; [Terminal ""]]);
 ("statements",
  [[Nonterminal "statement"; Nonterminal "delimeter_star"; Terminal ";";
    Nonterminal "delimeter_star"; Nonterminal "statements"];
   [Nonterminal "statement"; Nonterminal "delimeter_star"; Terminal ";";
    Nonterminal "delimeter_star"]]);
 ("else_branch",
  [[Terminal "else"; Nonterminal "delimeter_plus";
    Nonterminal "statement_sequence"]; [Terminal ""]]);
 ("declarations",
  [[Nonterminal "declaration"; Nonterminal "delimeter_star";
    Nonterminal "declarations"]; [Terminal ""]]);
 ("statements_in_block",
  [[Terminal "begin"; Nonterminal "delimeter_plus";
    Nonterminal "statement_sequence"]; [Terminal ""]]);
 ("formal_parameters_optional",
  [[Nonterminal "formal_parameters"]; [Terminal ""]]);
 ("imports",
  [[Nonterminal "import"; Nonterminal "delimeter_star"; Terminal ";";
    Nonterminal "delimeter_star"; Nonterminal "imports"]; [Terminal ""]]);
 ("letter",
  [[Terminal "a"]; [Terminal "b"]; [Terminal "c"]; [Terminal "d"];
   [Terminal "e"]; [Terminal "f"]; [Terminal "g"]; [Terminal "h"];
   [Terminal "i"]; [Terminal "j"]; [Terminal "k"]; [Terminal "l"];
   [Terminal "m"]; [Terminal "n"]; [Terminal "o"]; [Terminal "p"];
   [Terminal "q"]; [Terminal "r"]; [Terminal "s"]; [Terminal "t"];
   [Terminal "u"]; [Terminal "v"]; [Terminal "w"]; [Terminal "x"];
   [Terminal "y"]; [Terminal "z"]; [Terminal "A"]; [Terminal "B"];
   [Terminal "C"]; [Terminal "D"]; [Terminal "E"]; [Terminal "F"];
   [Terminal "G"]; [Terminal "H"]; [Terminal "I"]; [Terminal "J"];
   [Terminal "K"]; [Terminal "L"]; [Terminal "M"]; [Terminal "N"];
   [Terminal "O"]; [Terminal "P"]; [Terminal "Q"]; [Terminal "R"];
   [Terminal "S"]; [Terminal "T"]; [Terminal "U"]; [Terminal "V"];
   [Terminal "W"]; [Terminal "X"]; [Terminal "Y"]; [Terminal "Z"]]);
 ("digit",
  [[Terminal "0"]; [Terminal "1"]; [Terminal "2"]; [Terminal "3"];
   [Terminal "4"]; [Terminal "5"]; [Terminal "6"]; [Terminal "7"];
   [Terminal "8"]; [Terminal "9"]]); ("number", [[Nonterminal "digit_plus"]]);
 ("symbol", [[Nonterminal "letter"]; [Nonterminal "digit"]; [Terminal "_"]]);
 ("identifier", [[Nonterminal "symbol_plus"]]);
 ("delimeter", [[Terminal "\n"]; [Terminal " "]]);
 ("ident_list",
  [[Nonterminal "identifier"; Nonterminal "delimeter_star";
    Nonterminal "identifiers_tail"]]);
 ("qualident", [[Nonterminal "identifier"; Nonterminal "qualidents_tail"]]);
 ("import",
  [[Nonterminal "from_optional"; Terminal "import"; Nonterminal "delimeter_plus";
    Nonterminal "ident_list"]]);
 ("operation",
  [[Terminal "*"]; [Terminal "/"]; [Terminal "+"]; [Terminal "-"];
   [Terminal "div"]; [Terminal "mod"]; [Terminal "or"]; [Terminal "and"];
   [Terminal "="]; [Terminal "#"]; [Terminal "<"]; [Terminal "<="];
   [Terminal ">"]; [Terminal ">="]]);
 ("expr",
  [[Nonterminal "expr_no_lrec"; Nonterminal "delimeter_star";
    Nonterminal "operation"; Nonterminal "delimeter_star"; Nonterminal "expr"];
   [Nonterminal "expr_no_lrec"]]);
 ("expr_no_lrec",
  [[Terminal "("; Nonterminal "delimeter_star"; Nonterminal "expr";
    Nonterminal "delimeter_star"; Terminal ")"];
   [Terminal "not"; Nonterminal "delimeter_star"; Nonterminal "expr"];
   [Nonterminal "fn_call"]; [Nonterminal "number"];
   [Nonterminal "var_or_array_elem"];
   [Terminal "["; Nonterminal "delimeter_star"; Nonterminal "exp_list";
    Nonterminal "delimeter_star"; Terminal "]"]]);
 ("exp_list", [[Nonterminal "expr"; Nonterminal "exprs_tail"]]);
 ("fn_call",
  [[Nonterminal "qualident"; Terminal "("; Nonterminal "delimeter_star";
    Nonterminal "exp_list_optional"; Nonterminal "delimeter_star"; Terminal ")"]]);
 ("constant_declaration",
  [[Nonterminal "identifier"; Nonterminal "delimeter_star"; Terminal "=";
    Nonterminal "delimeter_star"; Nonterminal "expr"]]);
 ("variable_declaration",
  [[Nonterminal "ident_list"; Nonterminal "delimeter_star"; Terminal ":";
    Nonterminal "delimeter_star"; Nonterminal "qualident"]]);
 ("formal_parameter",
  [[Nonterminal "optional_var"; Nonterminal "delimeter_star";
    Nonterminal "ident_list"; Nonterminal "delimeter_star"; Terminal ":";
    Nonterminal "delimeter_star"; Nonterminal "qualident"]]);
 ("formal_parameter_list",
  [[Nonterminal "formal_parameter"; Nonterminal "formal_parameters_tail"]]);
 ("formal_parameters",
  [[Terminal "("; Nonterminal "delimeter_star";
    Nonterminal "formal_parameter_list_optional"; Nonterminal "delimeter_star";
    Terminal ")"; Nonterminal "optional_return_type"]]);
 ("type_def",
  [[Terminal "array["; Nonterminal "number"; Terminal "]";
    Nonterminal "delimeter_star"; Terminal "of"; Nonterminal "delimeter_plus";
    Nonterminal "identifier"];
   [Terminal "record"; Nonterminal "variable_declarations";
    Nonterminal "delimeter_plus"; Terminal "end"]]);
 ("declaration",
  [[Terminal "const"; Nonterminal "constant_declarations"];
   [Terminal "var"; Nonterminal "variable_declarations"];
   [Nonterminal "procedure_declaration"; Nonterminal "delimeter_star";
    Terminal ";"];
   [Terminal "type"; Nonterminal "delimeter_plus"; Nonterminal "typedefs"]]);
 ("var_or_array_elem",
  [[Nonterminal "identifier"; Terminal "["; Nonterminal "expr"; Terminal "]"];
   [Nonterminal "qualident"]]);
 ("statement",
  [[Nonterminal "var_or_array_elem"; Nonterminal "delimeter_star"; Terminal ":=";
    Nonterminal "delimeter_star"; Nonterminal "expr"];
   [Nonterminal "if_statement"]; [Nonterminal "while_statement"];
   [Terminal "return"; Nonterminal "optional_expr"]; [Nonterminal "fn_call"]]);
 ("statement_sequence", [[Nonterminal "statements"]]);
 ("if_statement",
  [[Terminal "if"; Nonterminal "delimeter_plus"; Nonterminal "expr";
    Nonterminal "delimeter_plus"; Terminal "then"; Nonterminal "delimeter_plus";
    Nonterminal "statement_sequence"; Nonterminal "else_branch";
    Nonterminal "delimeter_star"; Terminal "end"]]);
 ("while_statement",
  [[Terminal "while"; Nonterminal "delimeter_plus"; Nonterminal "expr";
    Nonterminal "delimeter_plus"; Terminal "do"; Nonterminal "delimeter_plus";
    Nonterminal "statement_sequence"; Terminal "end"]]);
 ("block",
  [[Nonterminal "declarations"; Nonterminal "statements_in_block";
    Nonterminal "delimeter_star"; Terminal "end"]]);
 ("procedure_declaration",
  [[Terminal "procedure"; Nonterminal "delimeter_plus"; Nonterminal "identifier";
    Nonterminal "delimeter_star"; Nonterminal "formal_parameters_optional";
    Terminal ";"; Nonterminal "delimeter_star"; Nonterminal "block";
    Nonterminal "delimeter_star"; Nonterminal "identifier"]]);
 ("modula_2",
  [[Terminal "module"; Nonterminal "delimeter_plus"; Nonterminal "identifier";
    Nonterminal "delimeter_star"; Terminal ";"; Nonterminal "delimeter_star";
    Nonterminal "imports"; Nonterminal "block"; Nonterminal "delimeter_star";
    Nonterminal "identifier"; Terminal "."; Nonterminal "delimeter_star"]])]


let parse' (result: (bool * obj array * int * (string * int)) * string) =
    let (failed, ast, _, _), syntax_error = result in
    if failed then
        printfn "%s" syntax_error
        None
    else
        let ast = ast[0] :?> Modula_2 in
        Some (ast)


let parse src measure_timedelta =
    parse' (parse "Modula_2" grammar src measure_timedelta)


let parse_file file measure_timedelta =
    parse' (parse_file file "Modula_2" grammar measure_timedelta)

