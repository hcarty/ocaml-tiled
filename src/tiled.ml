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

let is_in_bounds (tiled : t) ~row ~column : bool =
  Tmx.is_in_bounds tiled.tilemap ~row ~column

let get_tile (tiled : t) ~layer_id ~row ~column : Tmx.Tile.t =
  match Tmx.String_map.find_opt layer_id tiled.tilemap.layers with
  | None ->
    Fmt.invalid_arg "Layer id %s does not exist in %a" layer_id Fpath.pp
      tiled.tilemap.tileset_source
  | Some layer -> layer.tiles.(row).(column)

let get_tile_property (tiled : t) ~layer_id ~row ~column ~key :
    Tsx.Property.t option =
  let tile = get_tile tiled ~layer_id ~row ~column in
  Tsx.find_property tiled.tileset tile.index key

let get_tile_property_with (tiled : t) ~layer_id ~row ~column ~key ~f : _ option
    =
  let tile = get_tile tiled ~layer_id ~row ~column in
  Tsx.map_property tiled.tileset tile.index key f

let get_tile_property_bool = get_tile_property_with ~f:Tsx.Property.to_bool
let get_tile_property_string = get_tile_property_with ~f:Tsx.Property.to_string
let get_tile_property_int = get_tile_property_with ~f:Tsx.Property.to_int
let get_tile_property_float = get_tile_property_with ~f:Tsx.Property.to_float
let get_tile_property_color = get_tile_property_with ~f:Tsx.Property.to_color
