module Xml = Ezxmlm
module R = Rresult.R
module String_map = Map.Make (String)
module Int_map = Map.Make (Int)

module Property = struct
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
    | _ ->
      Format.kasprintf invalid_arg "%s is not a supported tile property type"
        type_

  let of_xml_exn xml =
    let xml_properties =
      Xml.member "properties" xml |> Xml.members_with_attr "property"
    in
    List.to_seq xml_properties
    |> Seq.map (fun (attrs, _) ->
           let name = Xml.get_attr "name" attrs in
           let type_ = Xml.get_attr "type" attrs in
           let value_ = Xml.get_attr "value" attrs in
           let property = of_strings type_ value_ in
           (name, property))
    |> String_map.of_seq
end

type dims = {
  width : int;
  height : int;
}

type t = {
  tile_size : dims;
  spacing : int;
  tile_count : int;
  columns : int;
  image_source : string;
  image_size : dims;
  tile_properties : Property.t String_map.t Int_map.t;
}

let get_int_attr name attrs = Xml.get_attr name attrs |> int_of_string

let load (path : Fpath.t) : t =
  let raw = Bos.OS.File.read path |> R.get_ok in
  let (_, xml) = Xml.from_string raw in
  let (attrs, tileset) = Xml.member_with_attr "tileset" xml in
  let tile_size : dims =
    let width = get_int_attr "tilewidth" attrs in
    let height = get_int_attr "tileheight" attrs in
    { width; height }
  in
  let spacing = get_int_attr "spacing" attrs in
  let tile_count = get_int_attr "tilecount" attrs in
  let columns = get_int_attr "columns" attrs in
  let (image_source, image_size) =
    let (attrs, _image) = Xml.member_with_attr "image" tileset in
    let image_source = Xml.get_attr "source" attrs in
    let image_size : dims =
      let width = get_int_attr "width" attrs in
      let height = get_int_attr "height" attrs in
      { width; height }
    in
    (image_source, image_size)
  in
  let tile_properties =
    let tile_xmls = Xml.members_with_attr "tile" tileset in
    List.fold_left
      (fun tiles (attrs, tile_xml) ->
        let id = get_int_attr "id" attrs in
        Int_map.add id (Property.of_xml_exn tile_xml) tiles)
      Int_map.empty tile_xmls
  in
  {
    tile_size;
    spacing;
    tile_count;
    columns;
    image_source;
    image_size;
    tile_properties;
  }

let get_tile_corner (tiles : t) i =
  let row = i / tiles.columns in
  let column = i - (tiles.columns * row) in
  let step dim = dim + tiles.spacing in
  (column * step tiles.tile_size.width, row * step tiles.tile_size.height)
