open Bfinterpreter

let _ =
  let bfinterp = create 100 "++++++++++++++++++++++++++++++++++++++++++++++++."
  in
  interpret bfinterp
