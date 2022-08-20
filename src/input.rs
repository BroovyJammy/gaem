use bevy::prelude::*;
use bevy_ecs_tilemap::prelude::*;
use leafwing_input_manager::{prelude::*, user_input::InputKind};

use crate::map::{select::CursorTilePos, Layer, LayerToMap};

#[derive(Actionlike, PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub enum Action {
    MoveSelection,
}

#[derive(Component, Copy, Clone)]
pub struct Player;

pub struct InputPlugin;
impl Plugin for InputPlugin {
    fn build(&self, app: &mut App) {
        // FIXME(Boxy) this should only run when in `GameState::Game` but i have no idea how
        // iyes_loopless works.
        app.add_system(handle_input.before(crate::map::select::highlight_hovered_tile))
            .add_system(camera_to_selected_tile.after(handle_input));
    }
}

pub fn make_action_manager() -> InputManagerBundle<Action> {
    InputManagerBundle {
        action_state: ActionState::default(),
        input_map: InputMap::default()
            .insert(DualAxis::left_stick(), Action::MoveSelection)
            .insert(
                VirtualDPad {
                    up: InputKind::GamepadButton(GamepadButtonType::DPadUp),
                    down: InputKind::GamepadButton(GamepadButtonType::DPadDown),
                    left: InputKind::GamepadButton(GamepadButtonType::DPadLeft),
                    right: InputKind::GamepadButton(GamepadButtonType::DPadRight),
                },
                Action::MoveSelection,
            )
            .insert(
                VirtualDPad {
                    up: InputKind::Keyboard(KeyCode::W),
                    down: InputKind::Keyboard(KeyCode::S),
                    left: InputKind::Keyboard(KeyCode::A),
                    right: InputKind::Keyboard(KeyCode::D),
                },
                Action::MoveSelection,
            )
            .build(),
    }
}

pub fn handle_input(
    mut actioners: Query<&ActionState<Action>>,
    tilemaps: Query<&TileStorage>,
    layer_to_map: Res<LayerToMap>,
    mut current_selected: ResMut<CursorTilePos>,
) {
    let actioner_state = actioners.get_single_mut().unwrap();

    // select an adjacent tile if applicable
    if actioner_state.just_pressed(Action::MoveSelection) {
        let movement = actioner_state
            .clamped_axis_pair(Action::MoveSelection)
            .unwrap() // no idea when this is None :shrug:
            .xy();

        if movement.length() >= 0.01 {
            let mut offset = IVec2::new(0, 0);
            if movement.x > 0.0 {
                offset.x += 1;
            } else if movement.x < 0.0 {
                offset.x -= 1;
            };
            if movement.y > 0.0 {
                offset.y += 1;
            } else if movement.y < -0.0 {
                offset.y -= 1;
            }
            let final_tile = current_selected.as_ivec2() + offset;
            // FIXME(Boxy) we ought to correctly handle the case where final_tile < 0 and
            // at the same time the case where `.get(..).is_some()` is fales but if we were
            // to go just left/right or up/down then `is_some()` would be true.
            assert!(final_tile.x >= 0);
            assert!(final_tile.y >= 0);
            let final_tile = final_tile.as_uvec2();

            let tileset_entity = layer_to_map.0[&Layer::Select];
            if tilemaps
                .get(tileset_entity)
                .unwrap()
                .get(&TilePos::new(final_tile.x, final_tile.y))
                .is_some()
            {
                current_selected.0 = final_tile;
            }
        }
    }
}

fn camera_to_selected_tile(
    mut set: ParamSet<(Query<&mut Transform, With<Camera>>, Query<&Transform>)>,
    layer_to_map: Res<LayerToMap>,
    selected_tile: Res<CursorTilePos>,
) {
    let map_entity = layer_to_map.0[&Layer::Select];
    let map_trans = *set.p1().get(map_entity).unwrap();
    let selected_tile_pos = map_trans.translation
        + (selected_tile.0 * UVec2::splat(crate::map::TILE_SIZE))
            .extend(0)
            .as_vec3();

    let mut temp = set.p0();
    let mut camera_trans = temp.get_single_mut().unwrap();
    // FIXME this probably sets `z` unnecessarily and also we should smooth move to the target
    // instead of directly jumping to it.
    camera_trans.translation = selected_tile_pos;
}
