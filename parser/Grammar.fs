module bnf.Grammar

open bnf.Parser
open Utils
open AstGenerator
open System
open System.IO


let letters = [ for letter in ([ 'A' .. 'Z' ] @ [ 'a' .. 'z' ]) -> [Terminal(string letter)] ]
let digits = [ for digit in [ '0' .. '9' ] -> [Terminal(string digit)] ]
let var_letter = letters @ digits @ [ [Terminal("_")] ]
let separators = [for delimeter in [" "; "\n"; "\t"; "\u000b"; "\f"] -> [Terminal(delimeter)]]


let tmp = [for char in ['`'; '-';
   '='; '['; ']'; '\\'; ';'; ','; '.'; '/'; '~'; '!';
   '@'; '#'; '$'; '%'; '^'; '&'; '*'; '('; ')'; '_'; '+';
   '{'; '}'; '|'; ':'; '"'; '<'; '>'; '?'] -> string char] @ ["\\'"]
let terminal_symbol = [for symbol in tmp -> [Terminal(symbol)]] @ separators @ letters @ digits


let grammar = [
      "var_symbol", var_letter // var_symbol = '0' | '1' | ... | 'x' | 'z'
      "var", [ // var = var_symbol var | var_symbol
       [ Nonterminal("var_symbol"); Nonterminal("var") ]
       [ Nonterminal("var_symbol") ]
      ]
      "terminal_symbol", terminal_symbol
      "terminal_symbols", [
          [Nonterminal("terminal_symbol"); Nonterminal("terminal_symbols")]
          [Terminal("")]
      ]
      "term", [                // term = "'" terminal_symbols "'"
          [ Terminal("'"); Nonterminal("terminal_symbols"); Terminal("'") ]
      ]
      "quantifier_or_nothing", [  // quantifier = '?' | '+' | '*' | ''
          [Terminal("?")]; [Terminal("+")]; [Terminal("*")]; [Terminal("")]
      ]
      "separator_symbol", separators     // separator_symbol = ' ' | '\n'
      "separator", [ // separator = separator_symbol separator | ''
        [ Nonterminal("separator_symbol"); Nonterminal("separator") ]
        [ Terminal("") ]
      ]
      "group_name", [ // group_name = '{' separator var separator '}' | ''
          [Terminal("{"); Nonterminal("separator"); Nonterminal("var"); Nonterminal("separator"); Terminal("}")]
          [Terminal("")]
      ]
      "expr", [ // expr = group separator expr | group
          [Nonterminal("group"); Nonterminal("separator"); Nonterminal("expr")]
          [Nonterminal("group")]
      ]
      "group", [
// group = '(' separator expr separator ')' group_name quantifier_or_nothing  | term group_name quantifier_or_nothing | nonterm quantifier_or_nothing
          [Terminal("("); Nonterminal("separator"); Nonterminal("expr"); Nonterminal("separator"); Terminal(")")
           Nonterminal("group_name")
           Nonterminal("quantifier_or_nothing")]
          [ Nonterminal("var"); Nonterminal("quantifier_or_nothing")]
          [ Nonterminal("term"); Nonterminal("group_name"); Nonterminal("quantifier_or_nothing")]
      ]
      "branch", [ // branch = group_or_items separator branch | group_or_items
          [Nonterminal("group"); Nonterminal("separator"); Nonterminal("branch")]
          [Nonterminal("group")]
      ]
      "branches", [  // branches = branch separator '|' separator branches | branch
        [Nonterminal("branch"); Nonterminal("separator"); Terminal("|"); Nonterminal("separator"); Nonterminal("branches")]
        [Nonterminal("branch")]
      ]
      "left_side", [ // left_side = var | '{' var '}'
          [Nonterminal("var")]
          [Terminal("{"); Nonterminal("var"); Terminal("}")] // means main terminal
      ]
      "equation", [ // equation = left_side separator '=' separator branches separator ';'
          [Nonterminal("left_side"); Nonterminal("separator"); Terminal("="); Nonterminal("separator")
           Nonterminal("branches"); Nonterminal("separator"); Terminal(";")]
      ]
      "bnf", [ // bnf = separator equation bnf | separator equation separator
          [ Nonterminal("separator"); Nonterminal("equation"); Nonterminal("bnf") ]
          [ Nonterminal("separator"); Nonterminal("equation"); Nonterminal("separator");]
      ]
]
