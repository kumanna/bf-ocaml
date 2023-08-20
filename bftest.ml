open Bfinterpreter

let _ =
  let bfinterp = create 1000 "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
  in
  interpret bfinterp
