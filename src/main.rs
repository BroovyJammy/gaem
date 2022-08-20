mod map;
pub mod state;
mod ui;

use bevy::prelude::*;
use map::MapPlugin;
use state::StatePlugin;
use ui::UiPlugin;

fn main() {
    App::new()
        // Ordered before, bc it adds `GameState`
        .add_plugin(StatePlugin)
        // Ordered before, bc it contains `WindowDescriptor`
        .add_plugin(UiPlugin)
        .add_plugins(DefaultPlugins)
        .add_plugin(MapPlugin)
        .run();
}
