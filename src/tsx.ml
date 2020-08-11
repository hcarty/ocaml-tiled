module R = Rresult.R
module String_map = Map.Make (String)
module Int_map = Map.Make (Int)

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

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

let load (path : Fpath.t) : t =
  let raw = Bos.OS.File.read path |> R.get_ok in
  let (_, xml) = Xml.from_string raw in
  let (attrs, tileset) = Xml.member_with_attr "tileset" xml in
  let tile_size : dims =
    let width = Xml.get_int_attr "tilewidth" attrs in
    let height = Xml.get_int_attr "tileheight" attrs in
    { width; height }
  in
  let spacing = Xml.get_int_attr "spacing" attrs in
  let tile_count = Xml.get_int_attr "tilecount" attrs in
  let columns = Xml.get_int_attr "columns" attrs in
  let (image_source, image_size) =
    let (attrs, _image) = Xml.member_with_attr "image" tileset in
    let image_source = Xml.get_attr "source" attrs in
    let image_size : dims =
      let width = Xml.get_int_attr "width" attrs in
      let height = Xml.get_int_attr "height" attrs in
      { width; height }
    in
    (image_source, image_size)
  in
  let tile_properties =
    let tile_xmls = Xml.members_with_attr "tile" tileset in
    List.fold_left
      (fun tiles (attrs, tile_xml) ->
        let id = Xml.get_int_attr "id" attrs in
        Int_map.add id (Property.of_xml tile_xml) tiles)
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

let find_properties (tiles : t) index =
  match Int_map.find_opt index tiles.tile_properties with
  | None -> String_map.empty
  | Some properties -> properties

let find_property (tiles : t) index key =
  let ( let* ) = Option.bind in
  let* properties = Int_map.find_opt index tiles.tile_properties in
  String_map.find_opt key properties

let map_property (tiles : t) index key f =
  let ( let* ) = Option.bind in
  let* property = find_property tiles index key in
  f property
