{
open Grammar

let keywords =
  [
    ("true", BOOL true);
    ("false", BOOL false);
    ("var", VAR);
    ("while", WHILE);
    ("if", IF);
    ("else", ELSE);
    ("def", DEF);
    ("return", RETURN);
    ("class", CLASS);
  ]

let keyword_lookup lexeme =
  match List.assoc_opt lexeme keywords with
  | Some tok -> tok
  | None -> NAME lexeme

}

let name = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let cname = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let int = ['0'-'9']['0'-'9' '_']*

rule token = parse
  | '#'               { comment lexbuf }
  | '\n'              { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\r' '\t']   { token lexbuf }
  | '"'               { read_string (Buffer.create 17) lexbuf }
  | name as n         { keyword_lookup n }
  | cname as n        { CNAME n }
  | int as i          { INT (int_of_string i) }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | '{'               { LBRACE }
  | '}'               { RBRACE }
  | '['               { LBRACK }
  | ']'               { RBRACK }
  | ','               { COMMA }
  | '.'               { DOT }
  | '-'               { MINUS }
  | '+'               { PLUS }
  | '*'               { STAR }
  | '/'               { FORWARD_SLASH }
  | '%'               { PERCENT }
  | '!'               { NOT }
  | "&&"              { LAND }
  | "||"              { LOR }
  | '<'               { LESS }
  | "<="              { LESSEQ }
  | '>'               { GREAT }
  | ">="              { GREATEQ }
  | "=="              { EQUAL }
  | "!="              { NEQUAL }
  | '='               { EQ }
  | "+="              { PLUSEQ }
  | "-="              { MINUSEQ }
  | "*="              { STAREQ }
  | eof               { EOF }

and comment = parse
  | '\n'              { Lexing.new_line lexbuf; token lexbuf }
  | _                 { comment lexbuf }
  | eof               { EOF }
and read_string buf = parse
  | '"'               { STRING (Buffer.contents buf) }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | eof
    { Error.syntax (Loc.of_lexeme lexbuf) "unterminated string" }

{
let read_file parser file =
  let c = open_in file in
  let lex = Lexing.from_channel c in
  Lexing.set_filename lex file;
  try
    let ast = parser lex in
    close_in c;
    ast
  with
    Error.Error e -> close_in c; raise (Error.Error e)
}
