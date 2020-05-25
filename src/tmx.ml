module Xml = Ezxmlm
module R = Rresult.R
module String_map = Map.Make (String)

module Tile = struct
  type t = {
    index : int;
    flip_horizontal : bool;
    flip_vertical : bool;
    flip_diagonal : bool;
  }

  (* Flags for conversion *)

  let flipped_horizontally_flag = 0x80000000

  let flipped_vertically_flag = 0x40000000

  let flipped_diagonally_flag = 0x20000000

  let of_int i : t =
    let flip_horizontal =
      i land flipped_horizontally_flag = flipped_horizontally_flag
    in
    let flip_vertical =
      i land flipped_vertically_flag = flipped_vertically_flag
    in
    let flip_diagonal =
      i land flipped_diagonally_flag = flipped_diagonally_flag
    in
    let index =
      i
      land lnot
             (flipped_horizontally_flag
             lor flipped_vertically_flag
             lor flipped_diagonally_flag
             )
      - 1
    in
    { index; flip_horizontal; flip_vertical; flip_diagonal }

  let pp ppf (tile : t) = Fmt.int ppf tile.index
end

module Layer = struct
  type t = {
    id : string;
    name : string;
    tiles : Tile.t array array;
  }

  let of_layer_xml (attrs : Xmlm.attribute list) (layer : Xml.nodes) : t =
    let tiles =
      Xml.member "data" layer
      |> Xml.data_to_string
      |> String.trim
      |> String.split_on_char '\n'
      |> List.map String.trim
      |> List.map (String.split_on_char ',')
      |> List.map (List.filter (fun s -> s <> ""))
      |> List.map (List.map (fun s -> Tile.of_int (int_of_string s)))
      |> Array.of_list
      |> Array.map Array.of_list
    in
    let id = Xml.get_attr "id" attrs in
    let name = Xml.get_attr "name" attrs in
    { id; name; tiles }
end

type dims = {
  width : int;
  height : int;
}

type t = {
  layers : Layer.t String_map.t;
  map_size : dims;
  tile_size : dims;
  tileset_source : Fpath.t;
}

let is_in_bounds (tmx : t) ~row ~column =
    let ({ width; height } : dims) = tmx.map_size in
    column >= 0 && column < width && row >= 0 && row < height

let load (path : Fpath.t) : t =
  let raw = Bos.OS.File.read path |> R.get_ok in
  let (_, xml) = Xml.from_string raw in
  let (attrs, map) = Xml.member_with_attr "map" xml in
  let map_size : dims =
    let width = Xml.get_attr "width" attrs |> int_of_string in
    let height = Xml.get_attr "height" attrs |> int_of_string in
    { width; height }
  in
  let tile_size : dims =
    let width = Xml.get_attr "tilewidth" attrs |> int_of_string in
    let height = Xml.get_attr "tileheight" attrs |> int_of_string in
    { width; height }
  in
  let tileset_source =
    let (attrs, _tileset) = Xml.member_with_attr "tileset" map in
    Xml.get_attr "source" attrs |> Fpath.v
  in
  let layers =
    let layer_xml = Xml.members_with_attr "layer" map |> List.to_seq in
    Seq.map (fun (attrs, xml) -> Layer.of_layer_xml attrs xml) layer_xml
    |> Seq.map (fun (layer : Layer.t) -> (layer.id, layer))
    |> String_map.of_seq
  in
  { layers; map_size; tile_size; tileset_source }
