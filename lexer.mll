{
open Parser
}

let white = [' ' '\t']*

rule read =
  parse
   white { read lexbuf }
  | ">" { MOVER }
  | "<" { MOVEL }
  | "+" { INCR }
  | "-" { DECR }
  | "[" { LOOPIN }
  | "]" { LOOPOUT }
  | "." { PRINT }
  | eof { EOF }
