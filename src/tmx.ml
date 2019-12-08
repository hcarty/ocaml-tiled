module Xml = Ezxmlm
module R = Rresult.R

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
    in
    { index; flip_horizontal; flip_vertical; flip_diagonal }

  let pp ppf (tile : t) = Fmt.int ppf tile.index
end

type dims = {
  width : int;
  height : int;
}

type t = {
  tiles : Tile.t array array;
  map_size : dims;
  tile_size : dims;
  tileset_source : Fpath.t;
}

let load (path : Fpath.t) : t =
  let raw = Bos.OS.File.read path |> R.get_ok in
  let _, xml = Xml.from_string raw in
  let attrs, map = Xml.member_with_attr "map" xml in
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
    let attrs, _tileset = Xml.member_with_attr "tileset" map in
    Xml.get_attr "source" attrs |> Fpath.v
  in
  let tiles =
    Xml.member "layer" map
    |> Xml.member "data"
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
  { tiles; map_size; tile_size; tileset_source }
