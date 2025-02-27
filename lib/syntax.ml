type name = string Loc.located

type cname = string Loc.located
and lit = lit' Loc.located

and lit' =
  | LInt of int
  | LStr of string
  | LBool of bool

and uop =
  | Neg
  | Not

and unary = unary' Loc.located
and unary' = uop * expr

and bop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Lt
  | Lte
  | Gt
  | Gte
  | Eq
  | Neq

and binary = binary' Loc.located
and binary' = bop * expr * expr
and array = array' Loc.located
and array' = expr list
and var = var' Loc.located

and var' =
  | Name of name
  | Subscript of var * expr
  | Field of var * name

and new_e = new_e' Loc.located
and new_e' = cname * expr list
and expr = expr' Loc.located

and expr' =
  | Lit of lit
  | Var of var
  | Unary of unary
  | Binary of binary
  | ArrayE of array
  | Call of call
  | New of new_e

and call = call' Loc.located
and call' = var * expr list
and declare = declare' Loc.located
and declare' = name * expr option
and assign = assign' Loc.located
and assign' = var * expr
and while_c = while_c' Loc.located
and while_c' = expr * block
and if_c = if_c' Loc.located
and if_c' = if_test * block * block option
and if_test =
  | Expr of expr
  | Dec of name * expr
and return = return' Loc.located
and return' = expr

and command =
  | CallC of call
  | DeclareC of declare
  | AssignC of assign
  | WhileC of while_c
  | IfC of if_c
  | Return of return
  | Block of block

and block = command list
and def = def' Loc.located
and def' = name * name list * block
and class_t = class_t' Loc.located
and class_t' = cname * name list * top_level list

and top_level =
  | Command of command
  | Def of def
  | Class of class_t
