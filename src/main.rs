#![allow(clippy::type_complexity, clippy::too_many_arguments)]

mod prelude {
    pub use bevy::prelude::*;
    pub use bevy::utils::{HashMap, HashSet};
    pub use bevy_ecs_tilemap::prelude::*;
    pub use bevy_kira_audio::prelude::*;
    pub use bevy_tweening::*;
    pub use iyes_loopless::prelude::*;
    pub use iyes_progress::prelude::*;
    pub use iyes_scene_tools::SceneBuilder;
    pub use leafwing_input_manager::prelude::*;

    pub use std::time::{Duration, Instant};

    pub use crate::AppState;
    pub use crate::{despawn_with, remove_resource};
}

mod asset;
mod cutscene;
mod dating;
mod gameplay;
mod ui;

mod scene_export;

use gameplay::{Team, Turn};

use crate::prelude::*;

#[derive(
    Clone, Copy, Debug, Eq, Hash, PartialEq, Default, Reflect, FromReflect, serde::Deserialize,
)]
pub enum AppState {
    Dating,
    #[default]
    AssetsLoading,
    MainMenu,
    Game,
    /// you must insert resource CurrentCutscene to select cutscene to play
    PlayCutscene,
    // dev tools / editors
    EditorCutscene,
    EditorLevelMap,
    EditorEzScene,
}

fn main() {
    let mut app = App::new();

    // general configuration
    app.insert_resource(WindowDescriptor {
        title: "BroovyJammy GÆM™ [PRE-ALPHA]".into(),
        present_mode: bevy::window::PresentMode::Fifo,
        resizable: true,
        width: 800.0,
        height: 600.0,
        resize_constraints: bevy::window::WindowResizeConstraints {
            min_width: 800.0,
            min_height: 600.0,
            ..Default::default()
        },
        ..Default::default()
    });
    #[cfg(debug_assertions)]
    app.insert_resource(bevy::log::LogSettings {
        filter: "info,wgpu_core=warn,wgpu_hal=warn,gaem=trace".into(),
        level: bevy::log::Level::TRACE,
    });
    #[cfg(not(debug_assertions))]
    app.insert_resource(bevy::log::LogSettings {
        filter: "info,wgpu_core=warn,wgpu_hal=warn,gaem=info".into(),
        level: bevy::log::Level::INFO,
    });

    // bevy
    app.add_plugins(DefaultPlugins);

    // our global things
    app.add_loopless_state(AppState::AssetsLoading);
    app.register_type::<AppState>();

    // external plugins
    app.add_plugin(ProgressPlugin::new(AppState::AssetsLoading));
    app.add_plugin(TilemapPlugin);
    app.add_plugin(InputManagerPlugin::<gameplay::Action>::default());

    // our plugins
    app.add_plugin(asset::AssetsPlugin);
    app.add_plugin(ui::UiPlugin);
    app.add_plugin(gameplay::GameplayPlugin);
    dating::add_self_to_app(&mut app);
    app.add_plugin(cutscene::CutscenePlugin);
    app.add_plugin(scene_export::SceneExportPlugin);
    gameplay::map::add_self_to_app(&mut app);

    // some debug diagnostics stuff
    #[cfg(debug_assertions)]
    {
        app.add_system(debug_state.exclusive_system().at_start());
        app.add_system(debug_nextstate.exclusive_system().at_end());
        app.add_system(debug_turn.exclusive_system().at_start());
    }

    // temporary for testing
    app.insert_resource(cutscene::CurrentCutscene::new("iyes finds god"));

    // let's gooo
    app.run();
}

fn debug_state(current: Res<CurrentState<AppState>>) {
    if current.is_changed() {
        debug!("State Changed! {:?}", *current);
    }
}

fn debug_nextstate(next: Option<Res<NextState<AppState>>>) {
    if let Some(next) = next {
        debug!("Queued state transition! {:?}", *next);
    }
}

fn debug_turn(current: Res<CurrentState<Turn>>) {
    if current.is_changed() {
        debug!(
            "{}",
            match current.0 {
                Turn(Team::Goodie) => "Your turn!",
                Turn(Team::Baddie) => "Enemy turn!",
            }
        );
    }
}

// some utils to help with cleanup

/// Despawn all entities with a specific marker component
///
/// Useful when exiting states
pub fn despawn_with<T: Component>(mut cmd: Commands, q: Query<Entity, With<T>>) {
    for e in q.iter() {
        cmd.entity(e).despawn_recursive();
    }
}

/// Remove a resource using Commands
pub fn remove_resource<T: Send + Sync + 'static>(mut cmd: Commands) {
    cmd.remove_resource::<T>();
}
