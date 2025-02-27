type t = Loc.t * string * string

exception Error of t

let error loc kind msg = raise (Error (loc, kind, msg))

let syntax loc msg = error loc "Syntax error" msg
let runtime loc msg = error loc "Runtime error" msg
