type t =
  { tape : bytes;
    length : int;
    current_position : int;
    code : string;
    code_pos : int;
  }

let create (length : int) (code : string) =
  { tape = Bytes.make length (char_of_int 0);
    length = length;
    current_position = 0;
    code = code;
    code_pos = 0;
  }

let increment_data_pointer bfstruct =
  if bfstruct.current_position < bfstruct.length - 1 then
    { bfstruct with current_position = bfstruct.current_position + 1 }
  else
    bfstruct

let decrement_data_pointer bfstruct =
  if bfstruct.current_position > 0 then
    { bfstruct with current_position = bfstruct.current_position - 1 }
  else
    bfstruct

let increment_value_at_pointer bfstruct =
  let current_value = Bytes.get bfstruct.tape bfstruct.current_position |> int_of_char in
  Bytes.set bfstruct.tape bfstruct.current_position (char_of_int (current_value + 1));
  bfstruct

let decrement_value_at_pointer bfstruct =
  let current_value = Bytes.get bfstruct.tape bfstruct.current_position |> int_of_char in
  Bytes.set bfstruct.tape bfstruct.current_position (char_of_int (current_value - 1));
  bfstruct

let print_current_value bfstruct =
  Printf.printf "%c" (Bytes.get bfstruct.tape bfstruct.current_position)

let tape_as_string bfstruct =
  String.of_bytes bfstruct.tape

let interpret bfstruct =
  let rec scan bfstruct =
    if bfstruct.code_pos >= String.length bfstruct.code then bfstruct else
    let c = bfstruct.code.[bfstruct.code_pos] in
      match c with
      | '+' -> scan { (increment_value_at_pointer bfstruct) with code_pos = bfstruct.code_pos + 1 }
      | '-' -> scan { (decrement_value_at_pointer bfstruct) with code_pos = bfstruct.code_pos + 1 }
      | '>' -> scan { (increment_data_pointer bfstruct) with code_pos = bfstruct.code_pos + 1 }
      | '<' -> scan { (decrement_data_pointer bfstruct) with code_pos = bfstruct.code_pos + 1 }
      | '.' -> print_current_value bfstruct ; scan { bfstruct with code_pos = bfstruct.code_pos + 1 }
      | _ -> failwith "Unhandled!"
  in
  bfstruct |> scan |> ignore
