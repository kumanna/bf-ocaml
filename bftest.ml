open Bfinterpreter

let test_zero () =
  let bf = create 1000 "++++++++++++++++++++++++++++++++++++++++++++++++" in
  interpret bf;
  Alcotest.(check string) "same tape" "0" (String.sub (Bfinterpreter.tape_as_string bf) 0 1)

let test_helloworld () =
  let bf = create 1000 "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++ .>>.<-.<.+++.------.--------.>>+.>++."
  in
  interpret bf;
  let template_string = "HdW!" in
  Alcotest.(check string) "same tape" template_string (String.sub (Bfinterpreter.tape_as_string bf) 2 (String.length template_string))


let () =
  let open Alcotest in
  run "Basic BF" [
      "increments", [ test_case "Test increments" `Slow test_zero ];
      "helloworld", [ test_case "Hello World!" `Slow test_helloworld ];
    ]
