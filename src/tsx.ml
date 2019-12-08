module Xml = Ezxmlm
module R = Rresult.R

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
}

let get_int_attr name attrs = Xml.get_attr name attrs |> int_of_string

let load (path : Fpath.t) : t =
  let raw = Bos.OS.File.read path |> R.get_ok in
  let _, xml = Xml.from_string raw in
  let attrs, tileset = Xml.member_with_attr "tileset" xml in
  let tile_size : dims =
    let width = get_int_attr "tilewidth" attrs in
    let height = get_int_attr "tileheight" attrs in
    { width; height }
  in
  let spacing = get_int_attr "spacing" attrs in
  let tile_count = get_int_attr "tilecount" attrs in
  let columns = get_int_attr "columns" attrs in
  let image_source, image_size =
    let attrs, _image = Xml.member_with_attr "image" tileset in
    let image_source = Xml.get_attr "source" attrs in
    let image_size : dims =
      let width = get_int_attr "width" attrs in
      let height = get_int_attr "height" attrs in
      { width; height }
    in
    (image_source, image_size)
  in
  { tile_size; spacing; tile_count; columns; image_source; image_size }

let get_tile_corner (tiles : t) i =
  let row = i / tiles.columns in
  let column = i - (tiles.columns * row) in
  let step dim = dim + tiles.spacing in
  (column * step tiles.tile_size.width, row * step tiles.tile_size.height)
