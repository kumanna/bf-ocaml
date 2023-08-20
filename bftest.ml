

let test_zero () =
  let bf = Bfinterpreter.create 1000 "++++++++++++++++++++++++++++++++++++++++++++++++" in
  Bfinterpreter.interpret bf;
  Alcotest.(check string) "same tape" "0" (String.sub (Bfinterpreter.tape_as_string bf) 0 1)

let test_pointermove () =
  let bf = Bfinterpreter.create 1000 ">>++++++++++++++++++++++++++++++++++++++++++++++++" in
  Bfinterpreter.interpret bf;
  Alcotest.(check string) "same tape" "0" (String.sub (Bfinterpreter.tape_as_string bf) 2 1)

let test_helloworld () =
  let bf = Bfinterpreter.create 1000 "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++ .>>.<-.<.+++.------.--------.>>+.>++."
  in
  Bfinterpreter.interpret bf;
  let template_string = "HdW!" in
  Alcotest.(check string) "same tape" template_string (String.sub (Bfinterpreter.tape_as_string bf) 2 (String.length template_string))

let test_zero_m () =
  let bf = Bfinterpreter2.create 1000 "++++++++++++++++++++++++++++++++++++++++++++++++" in
  Bfinterpreter2.interpret bf;
  Alcotest.(check string) "same tape" "0" (String.sub (Bfinterpreter2.tape_as_string bf) 0 1)

let test_pointermove_m () =
  let bf = Bfinterpreter2.create 1000 ">>++++++++++++++++++++++++++++++++++++++++++++++++" in
  Bfinterpreter2.interpret bf;
  Alcotest.(check string) "same tape" "0" (String.sub (Bfinterpreter2.tape_as_string bf) 2 1)

let test_helloworld_m () =
  let bf = Bfinterpreter2.create 1000 "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++ .>>.<-.<.+++.------.--------.>>+.>++."
  in
  Bfinterpreter2.interpret bf;
  let template_string = "HdW!" in
  Alcotest.(check string) "same tape" template_string (String.sub (Bfinterpreter2.tape_as_string bf) 2 (String.length template_string))


let () =
  let open Alcotest in
  run "Basic BF" [
      "increments", [ test_case "Test increments" `Slow test_zero ];
      "pointermove", [ test_case "Test pointer move" `Slow test_pointermove ];
      "helloworld", [ test_case "Hello World!" `Slow test_helloworld ];
      "increments_m", [ test_case "Test increments" `Slow test_zero_m ];
      "pointermove_m", [ test_case "Test pointer move" `Slow test_pointermove_m ];
      "helloworld_m", [ test_case "Hello World!" `Slow test_helloworld_m ];
    ]
