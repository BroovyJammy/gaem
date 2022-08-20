use bevy::prelude::*;
use bevy_ecs_tilemap::prelude::*;

use super::{
    tile::{Select, SelectTile},
    TILE_SIZE,
};

pub struct SelectPlugin;

impl Plugin for SelectPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(CursorTilePos {
            pos: UVec2::new(2, 2),
            snap_camera_to: true,
        })
        .add_system(update_cursor_pos)
        .add_system(highlight_hovered_tile.after(update_cursor_pos));
    }
}

// The position of the tile that the cursor is over
// I use `UVec2` instead of `TilePos` bc `UVec2` impls more useful traits
#[derive(Default)]
pub struct CursorTilePos {
    pub pos: UVec2,
    pub snap_camera_to: bool,
}

// Adapted from Star Machine, a game that's currently on hold
// Assumes that the map is at the origin
fn update_cursor_pos(
    cameras: Query<&Transform, With<Camera2d>>,
    mut cursor_movement: EventReader<CursorMoved>,
    windows: Res<Windows>,
    mut cursor: ResMut<CursorTilePos>,
) {
    let window = windows.get_primary().unwrap();
    match cursor_movement.iter().find(|event| event.id == window.id()) {
        Some(_) => (),
        None => return,
    };

    let new_pos = window.cursor_position().and_then(|cursor_pos| {
        cameras.get_single().ok().and_then(|camera_transform| {
            let new_pos = (camera_transform.compute_matrix()
                * (cursor_pos - Vec2::new(window.width(), window.height()) / 2.)
                    .extend(0.)
                    .extend(1.))
            .truncate()
            .truncate()
                / TILE_SIZE as f32;

            (new_pos.cmpge(Vec2::ZERO).all()).then(|| new_pos.as_uvec2())
        })
    });
    if let Some(new_pos) = new_pos {
        cursor.pos = new_pos;
    }
}

// Not optimized, but it keeps it simple
pub fn highlight_hovered_tile(
    mut tiles: Query<(&mut TileTexture, &TilePos), With<SelectTile>>,
    cursor_pos: Res<CursorTilePos>,
) {
    for (mut texture, &tile_pos) in &mut tiles {
        let tile_pos: UVec2 = tile_pos.into();
        *texture = match tile_pos == cursor_pos.pos {
            true => Select::Active,
            false => Select::Inactive,
        }
        .into();
    }
}
