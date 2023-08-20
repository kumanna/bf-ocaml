type t =
  { tape : bytes;
    length : int;
    current_position : int
  }

let create (length : int) =
  { tape = Bytes.make length (char_of_int 0);
    length = length;
    current_position = 0;
  }

let increment_data_pointer bfstruct =
  { bfstruct with current_position = bfstruct.current_position + 1 }

let decrement_data_pointer bfstruct =
  { bfstruct with current_position = bfstruct.current_position - 1 }

let increment_value_at_pointer bfstruct =
  let current_value = Bytes.get bfstruct.tape bfstruct.current_position |> int_of_char in
  Bytes.set bfstruct.tape bfstruct.current_position (char_of_int (current_value + 1))

let decrement_value_at_pointer bfstruct =
  let current_value = Bytes.get bfstruct.tape bfstruct.current_position |> int_of_char in
  Bytes.set bfstruct.tape bfstruct.current_position (char_of_int (current_value - 1))
