module modula_2.Ast

open RawAst
open bnf.Ast
open System.Collections.Generic

open LLVMSharp.Interop
open LLVMSharp



type BlockId = int


type Fields =
  Fields of (string * string) list // var * type
| FieldsOffsets of IDictionary<string, string * int> // var, type, offset,


type TypeDefinition =
  ArrayDef of elems_number : int *
              element_type : string

| StructDef of Fields // var * type

| Int

| Void




type Exp =
  Binary of Exp * string * Exp

| Unary of string * Exp

| FnCall' of name : string *
             args : Exp list

| Var of string

| Number of int

| ArrayValue of Exp list


| ArrayElemIndex of var       : string *
                    index_exp : Exp

| StructField of var    : string *
                 field  : string

// value1 = get(array0, index)
// array2 = update(array1, index, value2)

| Get of src            : Exp *
         field_or_index : IndexOrField

| Update of src            : Exp *
            field_or_index : IndexOrField *
            value          : Exp

| Local of id        : int *
           src_block : BlockId

| Phi of id        : int *
         src_block : BlockId *
         operands  : (int * BlockId) list

| NotFound

| Malloc of TypeDefinition

and IndexOrField =
  Index of Exp
| Field of string


type Statement =
  Assignment of Exp * Exp

| FnCall of name : string *
            args : Exp list

| If_then of cond        : Exp *
             then_branch : Statement list

| If_then_else of cond        : Exp *
                  then_branch : Statement list *
                  else_branch : Statement list

| While of cond : Exp *
           body : Statement list

| ReturnWithValue of Exp

| Return


type Block = Declaration list * Statement list

and Declaration =
  VarDecl of string * string // name * type

| Const of string * Exp

| Procedure of name        : string *
               args        : (string * string) list *  // (var_name * type) list
               return_type : string *
               body        : Block

| TypeDef of type_name   : string *
             type_def    : TypeDefinition


type Import =
  From of string * string list //
| Import of string list // from abc import efg | import abc


type Module =
    Module of module_name : string *
              imports     : Import list *
              block       : Block
