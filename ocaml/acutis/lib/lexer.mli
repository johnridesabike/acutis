exception SyntaxError
type state

val make_state : unit -> state
val acutis : state -> Lexing.lexbuf -> Parser.token
