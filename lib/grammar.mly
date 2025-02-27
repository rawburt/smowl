%{
open Syntax

let make_compound loc (name:name) (expr:expr) (op:bop) =
  let var = Loc.make (Name name) loc in
  let var_exp = Loc.make (Var var) loc in
  let binary' = (op, var_exp, expr) in
  let binary = Binary (Loc.make binary' loc) in
  let assign' = (var, (Loc.make binary loc)) in
  AssignC (Loc.make assign' loc)

%}
%token <string> NAME
%token <string> CNAME
%token <int> INT
%token <string> STRING
%token <bool> BOOL
%token VAR WHILE IF ELSE DEF RETURN CLASS
%token PLUS MINUS STAR FORWARD_SLASH PERCENT
%token PLUSEQ MINUSEQ STAREQ
%token NOT LAND LOR
%token LESS LESSEQ GREAT GREATEQ
%token EQUAL NEQUAL EQ
%token COMMA DOT
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token EOF

%right NOT
%left LOR
%left LAND
%nonassoc LESS LESSEQ GREAT GREATEQ EQUAL NEQUAL
%left PLUS MINUS
%left STAR PERCENT FORWARD_SLASH

%start <top_level list> top_level
%%

top_level: top_level_items EOF { $1 }

top_level_items: list(top_level_item) { $1 }
top_level_item:
  | command { Command $1 }
  | def { Def $1 }
  | class_tl { Class $1 }

def: located(def_) { $1 }
def_:
  | DEF name LPAREN separated_list(COMMA, name) RPAREN block { ($2, $4, $6) }

class_tl: located(class_tl_) { $1 }
class_tl_:
  | CLASS cname LPAREN separated_list(COMMA, name) RPAREN LBRACE top_level_items RBRACE { ($2, $4, $7) }

block: LBRACE list(command) RBRACE { $2 }

command:
  | call            { CallC $1 }
  | declare         { DeclareC $1 }
  | assign          { AssignC $1 }
  | compound_assign { $1 }
  | while_c         { WhileC $1 }
  | if_c            { IfC $1}
  | return          { Return $1 }
  | block           { Block $1 }

call: located(call_) { $1 }
call_:
  | var LPAREN separated_list(COMMA, expr) RPAREN { ($1, $3) }

declare: located(declare_) { $1 }
declare_:
  | VAR name EQ expr  { ($2, Some $4) }
  | VAR name          { ($2, None) }

assign: located(assign_) { $1 }
assign_:
  | var EQ expr { ($1, $3) }

compound_assign:
  | name PLUSEQ expr  { make_compound $startpos $1 $3 Add }
  | name MINUSEQ expr { make_compound $startpos $1 $3 Sub }
  | name STAREQ expr  { make_compound $startpos $1 $3 Mul }

while_c: located(while_c_) { $1 }
while_c_:
  | WHILE expr block { ($2, $3) }

if_c: located(if_c_) { $1 }
if_c_:
  | IF if_cond block ELSE block  { ($2, $3, Some $5) }
  | IF if_cond block ELSE if_c   { ($2, $3, Some [IfC $5]) }
  | IF if_cond block             { ($2, $3, None) }

if_cond:
  | expr          { Expr $1 }
  | name EQ expr  { Dec ($1, $3) }

return: located(return_) { $1 }
return_: RETURN expr { $2 }

expr: located(expr_) { $1 }
expr_:
  | lit     { Lit $1 }
  | var     { Var $1 }
  | unary   { Unary $1 }
  | binary  { Binary $1 }
  | call    { Call $1 }
  | array   { ArrayE $1 }
  | new_e   { New $1 }

lit: located(lit_) { $1 }
lit_:
  | INT     { LInt $1 }
  | STRING  { LStr $1 }
  | BOOL    { LBool $1 }

var: located(var_) { $1 }
var_:
  | name                    { Name $1 }
  | var LBRACK expr RBRACK  { Subscript ($1, $3) }
  | var DOT name            { Field ($1, $3) }

unary: located(unary_)  { $1 }
unary_: uop expr        { ($1, $2) }

%inline uop:
  | MINUS { Neg }
  | NOT   { Not }

binary: located(binary_)  { $1 }
binary_: expr bop expr    { ($2, $1, $3) }

%inline bop:
  | PLUS          { Add }
  | MINUS         { Sub }
  | STAR          { Mul }
  | FORWARD_SLASH { Div }
  | PERCENT       { Mod }
  | LAND          { And }
  | LOR           { Or }
  | LESS          { Lt }
  | LESSEQ        { Lte }
  | GREAT         { Gt }
  | GREATEQ       { Gte }
  | EQUAL         { Eq }
  | NEQUAL        { Neq }

array: located(array_) { $1 }
array_: LBRACK separated_list(COMMA, expr) RBRACK { $2 }

new_e: located(new_e_) { $1 }
new_e_:
  | cname LPAREN separated_list(COMMA, expr) RPAREN { ($1, $3) }

name:
  | located(NAME) { $1 }

cname:
  | located(CNAME) {$1 }

located(X):
  | X { Loc.make $1 $startpos }
