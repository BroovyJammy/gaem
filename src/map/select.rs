use bevy::prelude::*;
use bevy_ecs_tilemap::prelude::*;

use super::tile::{Select, SelectTile};

pub struct SelectPlugin;

impl Plugin for SelectPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(CursorTilePos(UVec2::new(2, 2)))
            .add_system(highlight_hovered_tile);
    }
}

// The position of the tile that the cursor is over
// I use `UVec2` instead of `TilePos` bc `UVec2` impls more useful traits
#[derive(Default, Deref, DerefMut)]
pub struct CursorTilePos(pub UVec2);

// Not optimized, but it keeps it simple
pub fn highlight_hovered_tile(
    mut tiles: Query<(&mut TileTexture, &TilePos), With<SelectTile>>,
    cursor_pos: Res<CursorTilePos>,
) {
    for (mut texture, &tile_pos) in &mut tiles {
        let tile_pos: UVec2 = tile_pos.into();
        *texture = match tile_pos == cursor_pos.0 {
            true => Select::Active,
            false => Select::Inactive,
        }
        .into();
    }
}
