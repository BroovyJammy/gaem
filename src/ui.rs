// Restructure as you like, just wanted to start this module so I can spawn a camera etc

use bevy::prelude::*;

pub struct UiPlugin;

impl Plugin for UiPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(WindowDescriptor {
            // Extremely permanent /s
            title: "an gay video gam e".into(),
            ..default()
        })
        .add_startup_system(main_menu);
    }
}

fn main_menu(mut commands: Commands) {
    commands.spawn_bundle(Camera2dBundle::default());
}
