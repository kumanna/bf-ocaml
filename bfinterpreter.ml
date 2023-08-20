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
