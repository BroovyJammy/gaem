mod asset;

use bevy::prelude::*;
use bevy_ecs_tilemap::prelude::*;
use iyes_loopless::prelude::AppLooplessStateExt;

use crate::state::GameState;

use self::asset::{MapAssetPlugin, MapAssets};

pub struct MapPlugin;

impl Plugin for MapPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugin(TilemapPlugin)
            .add_plugin(MapAssetPlugin)
            .add_enter_system(GameState::Game, init_map);
    }
}

// In tiles
const MAP_SIZE: u32 = 32;
// In pixels
const TILE_SIZE: u32 = 32;

// Based on https://github.com/StarArawn/bevy_ecs_tilemap/blob/main/examples/basic.rs
fn init_map(mut commands: Commands, assets: Res<MapAssets>) {
    let map = commands.spawn().id();

    let map_size = UVec2::splat(MAP_SIZE).into();
    let mut tile_storage = TileStorage::empty(map_size);

    for x in 0..MAP_SIZE {
        for y in 0..MAP_SIZE {
            let tile_pos = TilePos::new(x, y);
            tile_storage.set(
                &tile_pos,
                Some(
                    commands
                        .spawn()
                        .insert_bundle(TileBundle {
                            position: tile_pos,
                            tilemap_id: TilemapId(map),
                            ..default()
                        })
                        .id(),
                ),
            );
        }
    }

    let tile_size = Vec2::splat(TILE_SIZE as f32);
    // No `From<Vec2>` T_T
    commands.entity(map).insert_bundle(TilemapBundle {
        grid_size: TilemapGridSize {
            x: tile_size.x,
            y: tile_size.y,
        },
        size: map_size,
        storage: tile_storage,
        texture: TilemapTexture(assets.tiles.clone()),
        tile_size: TilemapTileSize {
            x: tile_size.x,
            y: tile_size.y,
        },
        ..default()
    });
}
