// Restructure as you like, just wanted to start this module so I can spawn a camera etc

use bevy::prelude::*;

pub struct UiPlugin;

impl Plugin for UiPlugin {
    fn build(&self, app: &mut App) {
        app.add_startup_system(main_menu);
    }
}

fn main_menu(mut commands: Commands) {
    commands
        .spawn_bundle(Camera2dBundle::default())
        .insert_bundle(crate::input::make_action_manager());
}
