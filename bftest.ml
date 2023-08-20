open Bfinterpreter

let _ =
  let bfinterp = create 100 in
  print_endline (tape_as_string bfinterp)
