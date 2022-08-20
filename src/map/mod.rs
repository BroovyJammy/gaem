mod asset;
mod tile;

use bevy::prelude::*;
use bevy_ecs_tilemap::prelude::*;
use iyes_loopless::prelude::AppLooplessStateExt;

use crate::state::GameState;

use self::{
    asset::{MapAssetPlugin, MapAssets},
    tile::{Select, Terrain},
};

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
// Characters will be at `1.`
const SELECT_LAYER_Z: f32 = 2.;
const TERRAIN_LAYER_Z: f32 = 0.;
// For some reason, this value has to be smaller than expected
const SELECT_LAYER_ALPHA: f32 = 0.1;

// Based on https://github.com/StarArawn/bevy_ecs_tilemap/blob/main/examples/basic.rs
fn init_map(mut commands: Commands, assets: Res<MapAssets>) {
    // Some common data between the layers
    let map_size = UVec2::splat(MAP_SIZE).into();
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

    for (tile_texture, z, layer_image, alpha) in [
        (
            Select::default().into(),
            SELECT_LAYER_Z,
            assets.select.clone(),
            SELECT_LAYER_ALPHA,
        ),
        (
            Terrain::Dirt.into(),
            TERRAIN_LAYER_Z,
            assets.terrain.clone(),
            1.,
        ),
    ] {
        let map = commands.spawn().id();
        let mut tile_storage = TileStorage::empty(map_size);

        // Can't use `fill_tilemap` bc we use a color (for select transparency)
        for x in 0..MAP_SIZE {
            for y in 0..MAP_SIZE {
                let tile_pos = UVec2::new(x, y).into();

                // Ime it's really messy for empty tiles to not have entities
                tile_storage.set(
                    &tile_pos,
                    Some(
                        commands
                            .spawn_bundle(TileBundle {
                                position: tile_pos,
                                tilemap_id: TilemapId(map),
                                texture: tile_texture,
                                color: TileColor(Vec3::ONE.extend(alpha).into()),
                                ..default()
                            })
                            .id(),
                    ),
                );
            }
        }

        commands.entity(map).insert_bundle(TilemapBundle {
            grid_size,
            size: map_size,
            storage: tile_storage,
            texture: TilemapTexture(layer_image),
            tile_size,
            transform: Transform::from_translation(Vec2::ZERO.extend(z)),
            ..default()
        });
    }
}
