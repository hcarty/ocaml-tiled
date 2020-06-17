module Xml = Ezxmlm
module String_map = Map.Make (String)

type t =
  | Bool of Bool.t
  | String of String.t
  | Int of Int.t
  | Float of Float.t
  | Color of String.t

let of_strings type_ value_ =
  match String.lowercase_ascii type_ with
  | "bool" -> Bool (bool_of_string value_)
  | "string" -> String value_
  | "int" -> Int (int_of_string value_)
  | "float" -> Float (float_of_string value_)
  | "color" -> Color value_
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
  | String _ -> "string"
  | Int _ -> "int"
  | Float _ -> "float"
  | Color _ -> "color"

let pp ppf (prop : t) =
  match prop with
  | Bool b -> Format.pp_print_bool ppf b
  | String s -> Format.pp_print_string ppf s
  | Int i -> Format.pp_print_int ppf i
  | Float f -> Format.pp_print_float ppf f
  | Color c -> Format.pp_print_string ppf c

let to_bool (property : t) =
  match property with
  | Bool b -> Some b
  | _ -> None

let to_string (property : t) =
  match property with
  | String s -> Some s
  | _ -> None

let to_int (property : t) =
  match property with
  | Int i -> Some i
  | _ -> None

let to_float (property : t) =
  match property with
  | Float f -> Some f
  | _ -> None

let to_color (property : t) =
  match property with
  | Color c -> Some c
  | _ -> None

let to_value_exn conv type_name (property : t) =
  match conv property with
  | None ->
    Fmt.invalid_arg "%a is %s, expected %s" pp property
      (to_type_string property) type_name
  | Some v -> v

let to_bool_exn = to_value_exn to_bool "bool"
let to_string_exn = to_value_exn to_string "string"
let to_int_exn = to_value_exn to_int "int"
let to_float_exn = to_value_exn to_float "float"
let to_color_exn = to_value_exn to_color "color"
