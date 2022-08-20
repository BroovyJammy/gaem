use crate::prelude::*;
use crate::{gameplay::MoveUnit, input::Action};

use super::{
    tile::{Select, SelectTile, UnitTile},
    Layer, LayerToMap, TILE_SIZE,
};

pub struct SelectPlugin;

impl Plugin for SelectPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(CursorTilePos {
            pos: UVec2::new(2, 2),
            snap_camera_to: true,
        })
        .add_event::<TileSelected>()
        .add_system(
            update_cursor_pos
                .run_in_state(AppState::Game)
                .after(crate::input::InputHandlingSystem)
                .label("update_cursor_pos"),
        )
        .add_system(
            highlight_hovered_tile
                .run_in_state(AppState::Game)
                .after("update_cursor_pos"),
        )
        .add_system(
            select_tile
                .run_in_state(AppState::Game)
                .after("update_cursor_pos"),
        )
        // TODO? eliminate 1-frame lag
        .add_system(move_unit.run_in_state(AppState::Game));
    }
}

// The position of the tile that the cursor is over
// I use `UVec2` instead of `TilePos` bc `UVec2` impls more useful traits
#[derive(Default)]
pub struct CursorTilePos {
    pub pos: UVec2,
    pub snap_camera_to: bool,
}

// Event that talks to the gameplay part
#[derive(Deref)]
pub struct TileSelected(UVec2);

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

fn select_tile(
    mut selecteds: EventWriter<TileSelected>,
    actioners: Query<&ActionState<Action>>,
    cursor_pos: Res<CursorTilePos>,
) {
    if actioners.single().just_pressed(Action::Select) {
        selecteds.send(TileSelected(cursor_pos.pos));
    }
}

fn move_unit(
    mut move_units: EventReader<MoveUnit>,
    tile_storages: Query<&TileStorage>,
    mut tiles: Query<&mut TileTexture, With<UnitTile>>,
    layer_to_map: Res<LayerToMap>,
) {
    for move_unit in move_units.iter() {
        let tile_storage = tile_storages
            .get(*layer_to_map.get(&Layer::Unit).unwrap())
            .unwrap();

        // Let's just assume the gameplay end knows the units aren't off the board
        let from = tile_storage.get(&move_unit.from.into()).unwrap();
        let to = tile_storage.get(&move_unit.to.into()).unwrap();

        // Ah, a good old fashioned swap. No `mem::swap` today.
        let temp = *tiles.get(from).unwrap();
        *tiles.get_mut(from).unwrap() = *tiles.get(to).unwrap();
        *tiles.get_mut(to).unwrap() = temp;
    }
}
