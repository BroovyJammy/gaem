use bevy::prelude::*;
use bevy_ecs_tilemap::prelude::*;
use iyes_loopless::prelude::IntoConditionalSystem;
use leafwing_input_manager::prelude::*;

use crate::{input::Action, state::GameState};

use super::{
    tile::{Select, SelectTile},
    TILE_SIZE,
};

pub struct SelectPlugin;

impl Plugin for SelectPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<CursorTilePos>()
            .add_event::<TileSelected>()
            .add_system(
                update_cursor_pos
                    .run_in_state(GameState::Game)
                    .label("update_cursor_pos"),
            )
            .add_system(
                highlight_hovered_tile
                    .run_in_state(GameState::Game)
                    .after("update_cursor_pos"),
            )
            .add_system(
                select_tile
                    .run_in_state(GameState::Game)
                    .after("update_cursor_pos"),
            );
    }
}

// The position of the tile that the cursor is over
// I use `UVec2` instead of `TilePos` bc `UVec2` impls more useful traits
#[derive(Default, Deref, DerefMut)]
pub struct CursorTilePos(Option<UVec2>);

// Event that talks to the gameplay part
#[derive(Deref)]
pub struct TileSelected(UVec2);

// Adapted from Star Machine, a game that's currently on hold
// Assumes that the map is at the origin
fn update_cursor_pos(
    cameras: Query<&Transform, With<Camera2d>>,
    windows: Res<Windows>,
    mut cursor: ResMut<CursorTilePos>,
) {
    let window = windows.get_primary().unwrap();
    **cursor = window.cursor_position().and_then(|cursor_pos| {
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
}

// Not optimized, but it keeps it simple
fn highlight_hovered_tile(
    mut tiles: Query<(&mut TileTexture, &TilePos), With<SelectTile>>,
    cursor_pos: Res<CursorTilePos>,
) {
    for (mut texture, tile_pos) in &mut tiles {
        *texture = if Some(tile_pos.into()) == **cursor_pos {
            Select::Active
        } else {
            Select::Inactive
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
        if let Some(pos) = **cursor_pos {
            selecteds.send(TileSelected(pos));
        }
    }
}
