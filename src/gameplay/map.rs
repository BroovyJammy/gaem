use noise::{Add, Constant, Fbm, NoiseFn, Power, Seedable};
use rand::Rng;

use crate::asset::{MapAssets, Terrain};
use crate::prelude::*;

use super::LevelInfo;

#[derive(Hash, Eq, Clone, Component, Copy, PartialEq)]
pub enum Layer {
    Select,
    Movement,
    Terrain,
}

#[derive(Deref)]
pub struct LayerToMap(pub HashMap<Layer, Entity>);

#[derive(Component)]
pub struct SelectTile;

// Should follow the order of `image/tile/select.png`. Converts to `TileTexture` with `as u32`.
#[derive(Default)]
pub enum Select {
    Inactive,
    #[default]
    Active,
}

impl From<Select> for TileTexture {
    fn from(select: Select) -> Self {
        Self(select as u32)
    }
}

#[derive(Component)]
pub struct MovementTile;

// See `image/tile/unit.png`
pub enum Unit {
    None,
    Unit,
}

impl From<Unit> for TileTexture {
    fn from(unit: Unit) -> Self {
        Self(unit as u32)
    }
}

#[derive(Clone, Component, Copy, Debug, Eq, PartialEq)]
pub struct TerrainKind(pub usize);

#[derive(Component)]
pub struct TerrainTile;

// In pixels
pub const TILE_SIZE: u32 = 32;
// Characters will be at `1.`
const SELECT_LAYER_Z: f32 = 2.;
const MOVEMENT_LAYER_Z: f32 = 1.;
const TERRAIN_LAYER_Z: f32 = 0.;
// For some reason, this value has to be smaller than expected
const SELECT_LAYER_ALPHA: f32 = 0.4;

// Based on https://github.com/StarArawn/bevy_ecs_tilemap/blob/main/examples/basic.rs
pub fn init_map(
    mut commands: Commands,
    assets: Res<MapAssets>,
    terrain: Res<Terrain>,
    level_info: LevelInfo<'_, '_>,
) {
    // Some common data between the layers
    let map_size: TilemapSize =
        UVec2::new(level_info.level().size_x, level_info.level().size_y).into();
    let tile_size = Vec2::splat(TILE_SIZE as f32);
    // No `From<Vec2>` T_T
    let grid_size = TilemapGridSize {
        x: tile_size.x,
        y: tile_size.y,
    };
    let tile_size = TilemapTileSize {
        x: tile_size.x,
        y: tile_size.y,
    };

    struct EdgeWall(UVec2);

    impl NoiseFn<[f64; 2]> for EdgeWall {
        fn get(&self, mut point: [f64; 2]) -> f64 {
            for dim in &mut point {
                if *dim > (self.0.x / 2) as f64 {
                    *dim = self.0.x as f64 - *dim;
                }
            }

            (1. - (point[0].min(point[1]) / (self.0.x / 2) as f64)) * 1.1
        }
    }

    let mut fbm = Fbm::new();
    fbm.frequency = 0.1;
    let fbm: &dyn NoiseFn<[f64; 2]> = &fbm.set_seed(rand::thread_rng().gen());
    let constant = Constant::new(-0.3);
    let add = Add::new(fbm, &constant);

    let constant = Constant::new(8.);
    let stupid_rustc = EdgeWall(UVec2::new(map_size.x, map_size.y));
    let power = Power::new(&stupid_rustc, &constant);

    let noise = Add::new(&add, &power);

    let mut layer_to_map = LayerToMap(HashMap::new());
    for layer in [Layer::Select, Layer::Movement, Layer::Terrain] {
        let map = commands.spawn().id();
        let mut tile_storage = TileStorage::empty(map_size);

        // Can't use `fill_tilemap` bc we use a color (for select transparency)
        for x in 0..map_size.x {
            for y in 0..map_size.y {
                let tile_pos = UVec2::new(x, y).into();

                let terrain_kind = TerrainKind(
                    match x <= 1 || x >= (map_size.x - 2) || y <= 1 || y >= (map_size.y - 2) {
                        false => 0,
                        true => 1,
                    },
                );

                // let terrain_kind = TerrainKind(match noise.get([x as f64, y as f64]) > 0. {
                //     false => 0,
                //     true => 0,
                //     // true => 1,
                // });

                let mut tile = commands.spawn_bundle(TileBundle {
                    position: tile_pos,
                    tilemap_id: TilemapId(map),
                    // TEMP We'll want to load it from some data eventually
                    texture: match layer {
                        // Movement is like the select layer but green
                        // They should be separate tho, so you can have both tiles at the same spot
                        Layer::Select | Layer::Movement => Select::Inactive.into(),
                        Layer::Terrain => TileTexture(terrain[terrain_kind].sprite_idx as u32),
                    },
                    color: TileColor(match layer {
                        Layer::Select => Vec3::ONE.extend(SELECT_LAYER_ALPHA).into(),
                        Layer::Movement => Color::rgba(0.5, 1., 0.5, SELECT_LAYER_ALPHA),
                        _ => Color::WHITE,
                    }),
                    ..default()
                });

                match layer {
                    Layer::Select => {
                        tile.insert(SelectTile);
                    }
                    Layer::Movement => {
                        tile.insert(MovementTile);
                    }
                    Layer::Terrain => {
                        tile.insert(TerrainTile);
                    }
                }
                tile.insert(terrain_kind);

                // Ime it's really messy for empty tiles to not have entities
                tile_storage.set(&tile_pos, Some(tile.id()));
            }
        }

        commands.entity(map).insert_bundle(TilemapBundle {
            grid_size,
            size: map_size,
            storage: tile_storage,
            texture: TilemapTexture(match layer {
                Layer::Select | Layer::Movement => assets.select.clone(),
                Layer::Terrain => assets.terrain.clone(),
            }),
            tile_size,
            transform: Transform::from_translation(Vec2::ZERO.extend(match layer {
                Layer::Select => SELECT_LAYER_Z,
                Layer::Movement => MOVEMENT_LAYER_Z,
                Layer::Terrain => TERRAIN_LAYER_Z,
            })),
            ..default()
        });
        layer_to_map.0.insert(layer, map);
    }
    commands.insert_resource(layer_to_map);
}
