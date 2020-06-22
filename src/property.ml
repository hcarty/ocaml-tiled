module Xml = Ezxmlm
module String_map = Map.Make (String)

type t =
  | Bool of Bool.t
  | Color of String.t
  | File of String.t
  | Float of Float.t
  | Int of Int.t
  | Object of Int.t
  | String of String.t

let of_strings type_ value_ =
  match String.lowercase_ascii type_ with
  | "bool" -> Bool (bool_of_string value_)
  | "color" -> Color value_
  | "file" -> File value_
  | "float" -> Float (float_of_string value_)
  | "int" -> Int (int_of_string value_)
  | "object" -> Object (int_of_string value_)
  | "string" -> String value_
  | _ -> Fmt.invalid_arg "%s is not a supported tile property type" type_

let of_xml_exn xml =
  let xml_properties =
    Xml.member "properties" xml |> Xml.members_with_attr "property"
  in
  List.to_seq xml_properties
  |> Seq.map (fun (attrs, _) ->
         let name = Xml.get_attr "name" attrs in
         let type_ =
           match Xml.get_attr "type" attrs with
           | x -> x
           | exception Not_found -> "string"
         in
         let value_ = Xml.get_attr "value" attrs in
         let property = of_strings type_ value_ in
         (name, property))
  |> String_map.of_seq

let of_xml xml =
  match of_xml_exn xml with
  | exception Xml.Tag_not_found _ -> String_map.empty
  | x -> x

let to_type_string (prop : t) =
  match prop with
  | Bool _ -> "bool"
  | Color _ -> "color"
  | File _ -> "file"
  | Float _ -> "float"
  | Int _ -> "int"
  | Object _ -> "object"
  | String _ -> "string"

let pp ppf (prop : t) =
  match prop with
  | Bool b -> Format.pp_print_bool ppf b
  | Color c -> Format.pp_print_string ppf c
  | Int i -> Format.pp_print_int ppf i
  | File f -> Format.pp_print_string ppf f
  | Float f -> Format.pp_print_float ppf f
  | Object o -> Format.pp_print_int ppf o
  | String s -> Format.pp_print_string ppf s

let to_bool (property : t) =
  match property with
  | Bool b -> Some b
  | _ -> None

let to_color (property : t) =
  match property with
  | Color c -> Some c
  | _ -> None

let to_file (property : t) =
  match property with
  | File f -> Some f
  | _ -> None

let to_float (property : t) =
  match property with
  | Float f -> Some f
  | _ -> None

let to_int (property : t) =
  match property with
  | Int i -> Some i
  | _ -> None

let to_object (property : t) =
  match property with
  | Object o -> Some o
  | _ -> None

let to_string (property : t) =
  match property with
  | String s -> Some s
  | _ -> None

let to_value_exn conv type_name (property : t) =
  match conv property with
  | None ->
    Fmt.invalid_arg "%a is %s, expected %s" pp property
      (to_type_string property) type_name
  | Some v -> v

let to_bool_exn = to_value_exn to_bool "bool"
let to_color_exn = to_value_exn to_color "color"
let to_file_exn = to_value_exn to_file "file"
let to_float_exn = to_value_exn to_float "float"
let to_int_exn = to_value_exn to_int "int"
let to_object_exn = to_value_exn to_object "object"
let to_string_exn = to_value_exn to_string "string"
