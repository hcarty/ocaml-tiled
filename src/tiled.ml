module Tsx = Tsx
module Tmx = Tmx

type t = {
  tileset : Tsx.t;
  tilemap : Tmx.t;
}

let load_map map_path : t =
  let tilemap = Tmx.load map_path in
  let tiles_path =
    if Fpath.is_abs tilemap.tileset_source then
      tilemap.tileset_source
    else (
      let (map_dir, _) = Fpath.split_base map_path in
      Fpath.append map_dir tilemap.tileset_source
    )
  in
  let tileset = Tsx.load tiles_path in
  { tileset; tilemap }
