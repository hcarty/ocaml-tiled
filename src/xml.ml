include Ezxmlm

let get_int_attr name attrs = get_attr name attrs |> int_of_string
let get_float_attr name attrs = get_attr name attrs |> float_of_string
