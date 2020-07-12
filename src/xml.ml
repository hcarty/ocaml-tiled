include Ezxmlm

let get_int_attr name attrs = get_attr name attrs |> int_of_string
let get_float_attr name attrs = get_attr name attrs |> float_of_string

let get_int_attr_opt name attrs =
  match get_int_attr name attrs with
  | exception Not_found -> None
  | i -> Some i
