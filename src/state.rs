use bevy::prelude::*;
use iyes_loopless::prelude::AppLooplessStateExt;

pub struct StatePlugin;

impl Plugin for StatePlugin {
    fn build(&self, app: &mut App) {
        app.add_loopless_state(GameState::Game);
    }
}

// Feel free to move this
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum GameState {
    Game,
}
