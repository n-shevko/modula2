module bnf.Ast


open Utils


type Exp =
  TermOrNonterm of item       : Item *
                   name       : string *
                   quantifier : string

| Group of content    : Exp list *
           name       : string *
           quantifier : string


type Branch = Exp list


type Branches = Branch list


type LeftSide =
  EntryPoint of string
| UsualNonterm of string


type Equation = LeftSide * Branches


type Equations = Equation list