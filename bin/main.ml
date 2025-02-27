open Smowl

let parse lexbuf =
  try Grammar.top_level Lexer.token lexbuf with
  | Grammar.Error ->
      Error.syntax (Loc.of_lexeme lexbuf)
        ("parser error at token: " ^ Lexing.lexeme lexbuf)
  | Failure failmsg when failmsg = "lexing: empty token" ->
      Error.syntax (Loc.of_lexeme lexbuf) "unrecognised symbol"

let () =
  let f = Sys.argv.(1) in
  try
    let top_levels = Lexer.read_file parse f in
    (* Eval.eval top_level *)
    Interpreter.Eval.run top_levels
  with
  | Error.Error (loc, kind, msg) ->
      Printf.printf "%s (%s):\n%s" kind (Loc.to_string loc) msg
