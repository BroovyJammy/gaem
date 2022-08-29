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
mod gameplay;
mod ui;

mod scene_export;

use gameplay::{Team, Turn};

use crate::prelude::*;

#[derive(
    Clone, Copy, Debug, Eq, Hash, PartialEq, Default, Reflect, FromReflect, serde::Deserialize,
)]
pub enum AppState {
    #[default]
    AssetsLoading,
    MainMenu,
    Game,
    InsectCombiner,
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
        title: "Spindarella's Monsters".into(),
        present_mode: bevy::window::PresentMode::Fifo,
        resizable: true,
        width: 1200.0,
        height: 900.0,
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
    app.insert_resource(ClearColor(Color::rgb(0.04, 0.05, 0.06)));

    // bevy
    app.add_plugins(DefaultPlugins);

    // our global things
    app.add_loopless_state(AppState::AssetsLoading);
    app.register_type::<AppState>();
    app.add_stage_before(CoreStage::Update, "fuckstages", SystemStage::parallel());

    // external plugins
    app.add_plugin(ProgressPlugin::new(AppState::AssetsLoading));
    app.add_plugin(TilemapPlugin);
    app.add_plugin(InputManagerPlugin::<gameplay::Action>::default());
    app.add_plugin(AudioPlugin);

    // our plugins
    app.add_plugin(asset::AssetsPlugin);
    app.add_plugin(ui::UiPlugin);
    app.add_plugin(gameplay::GameplayPlugin);
    app.add_plugin(cutscene::CutscenePlugin);
    app.add_plugin(scene_export::SceneExportPlugin);

    // some debug diagnostics stuff
    #[cfg(debug_assertions)]
    {
        app.add_system(debug_state.exclusive_system().at_start());
        app.add_system(debug_nextstate.exclusive_system().at_end());
        app.add_system(debug_turn.exclusive_system().at_start());
    }

    app.insert_resource(gameplay::CurrentLevel(0));
    app.insert_resource(gameplay::Levels(vec![
        gameplay::Level {
            size_x: 36,
            size_y: 36,
            post_cutscene: Some("01".into()),
            player_spawn_points: vec![UVec2::new(7, 7), UVec2::new(17, 17)],
            enemy_spawn_points: vec![(UVec2::new(29, 29), 1)],
        },
        gameplay::Level {
            size_x: 36,
            size_y: 36,
            post_cutscene: Some("02".into()),
            player_spawn_points: vec![UVec2::new(7, 12), UVec2::new(22, 12)],
            enemy_spawn_points: vec![(UVec2::new(5, 30), 1), (UVec2::new(25, 30), 1)],
        },
        gameplay::Level {
            size_x: 36,
            size_y: 36,
            post_cutscene: Some("03".into()),
            player_spawn_points: vec![UVec2::new(7, 12), UVec2::new(22, 12), UVec2::new(7, 22)],
            enemy_spawn_points: vec![(UVec2::new(26, 21), 2)],
        },
        gameplay::Level {
            size_x: 52,
            size_y: 52,
            post_cutscene: Some("04".into()),
            player_spawn_points: vec![UVec2::new(7, 12), UVec2::new(22, 12), UVec2::new(37, 12)],
            enemy_spawn_points: vec![(UVec2::new(26, 21), 2), (UVec2::new(17, 21), 1)],
        },
        gameplay::Level {
            size_x: 52,
            size_y: 52,
            post_cutscene: Some("05".into()),
            player_spawn_points: vec![
                UVec2::new(7, 12),
                UVec2::new(17, 12),
                UVec2::new(27, 12),
                UVec2::new(37, 12),
            ],
            enemy_spawn_points: vec![(UVec2::new(24, 19), 3)],
        },
        gameplay::Level {
            size_x: 52,
            size_y: 52,
            post_cutscene: Some("06".into()),
            player_spawn_points: vec![
                UVec2::new(7, 12),
                UVec2::new(17, 12),
                UVec2::new(27, 12),
                UVec2::new(37, 12),
            ],
            enemy_spawn_points: vec![
                (UVec2::new(42, 42), 1),
                (UVec2::new(37, 42), 1),
                (UVec2::new(32, 42), 1),
                (UVec2::new(27, 42), 1),
                (UVec2::new(42, 37), 1),
                (UVec2::new(37, 37), 1),
                (UVec2::new(32, 37), 1),
                (UVec2::new(27, 37), 1),
            ],
        },
        gameplay::Level {
            size_x: 52,
            size_y: 52,
            post_cutscene: Some("07".into()),
            player_spawn_points: vec![
                UVec2::new(7, 12),
                UVec2::new(17, 12),
                UVec2::new(27, 12),
                UVec2::new(37, 12),
            ],
            enemy_spawn_points: vec![
                (UVec2::new(42, 42), 2),
                (UVec2::new(32, 42), 2),
                (UVec2::new(22, 42), 2),
                (UVec2::new(12, 37), 2),
            ],
        },
        gameplay::Level {
            size_x: 52,
            size_y: 52,
            post_cutscene: Some("08".into()),
            player_spawn_points: vec![
                UVec2::new(7, 12),
                UVec2::new(17, 12),
                UVec2::new(27, 12),
                UVec2::new(37, 12),
            ],
            enemy_spawn_points: vec![(UVec2::new(37, 35), 3), (UVec2::new(22, 35), 3)],
        },
        gameplay::Level {
            size_x: 52,
            size_y: 52,
            post_cutscene: Some("09".into()),
            player_spawn_points: vec![
                UVec2::new(7, 12),
                UVec2::new(17, 12),
                UVec2::new(27, 12),
                UVec2::new(37, 12),
            ],
            enemy_spawn_points: vec![(UVec2::new(26, 26), 5)],
        },
        gameplay::Level {
            size_x: 68,
            size_y: 68,
            post_cutscene: Some("10".into()),
            player_spawn_points: vec![
                UVec2::new(7, 12),
                UVec2::new(17, 12),
                UVec2::new(27, 12),
                UVec2::new(37, 12),
            ],
            enemy_spawn_points: vec![(UVec2::new(20, 40), 4), (UVec2::new(40, 40), 4)],
        },
    ]));

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
                Turn::InputActions(Team::Goodie) => "Your turn!",
                Turn::InputActions(Team::Baddie) => "Enemy turn!",
                Turn::AnimateActions(Team::Goodie) => "Executing your turn!",
                Turn::AnimateActions(Team::Baddie) => "Executirng enemy turn!",
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
