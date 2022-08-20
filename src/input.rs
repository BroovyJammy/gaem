use bevy::prelude::*;
use leafwing_input_manager::{prelude::*, user_input::InputKind};

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
        app.add_system(handle_input);
    }
}

pub fn make_action_manager() -> InputManagerBundle<Action> {
    InputManagerBundle {
        action_state: ActionState::default(),
        input_map: InputMap::default()
            .insert(DualAxis::left_stick(), Action::MoveSelection)
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

pub fn handle_input(mut actioners: Query<(&mut Transform, &ActionState<Action>)>) {
    const CAM_SPEED: f32 = 2.;

    let (mut actioner_trans, actioner_state) = actioners.get_single_mut().unwrap();
    if actioner_state.pressed(Action::MoveSelection) {
        let movement = actioner_state
            .clamped_axis_pair(Action::MoveSelection)
            .unwrap() // no idea when this is None :shrug:
            .xy();

        if movement.length() >= 0.1 {
            actioner_trans.translation += movement.extend(0.0) * CAM_SPEED;
        }
    }
}
