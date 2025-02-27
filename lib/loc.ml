type t = {
  file : string;
  line : int;
  column : int;
}

type 'a located = {
  at : t;
  it : 'a;
}

let to_string { file; line; column } =
  Printf.sprintf "file %S, line %d, char %d" file line column

let of_lexing_pos lexpos : t =
  let file = lexpos.Lexing.pos_fname in
  let line = lexpos.Lexing.pos_lnum in
  let column = lexpos.Lexing.pos_cnum - lexpos.Lexing.pos_bol + 1 in
  { file; line; column }

let of_lexeme lexbuf : t = of_lexing_pos lexbuf.Lexing.lex_curr_p
let make it pos = { at = of_lexing_pos pos; it }
