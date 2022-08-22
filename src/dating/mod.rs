use bevy::sprite::Anchor;

use crate::{asset::UiAssets, prelude::*};

#[derive(Component)]
pub struct DatingSimEntity;

pub fn add_self_to_app(app: &mut App) {
    app.add_enter_system(AppState::Dating, setup_dating);
    app.add_exit_system(AppState::Dating, clear_dating_sim_entities);
    app.add_system(dating.run_in_state(AppState::Dating));
}

pub fn setup_dating(
    mut cmds: Commands<'_, '_>,
    assets: Res<UiAssets>,
    windows: Res<Windows>,
    mut camera: Query<&mut Transform, With<Camera>>,
) {
    *camera.single_mut() = Transform::default();

    let window = windows.get_primary().unwrap();

    cmds.spawn_bundle(SpriteBundle {
        sprite: Sprite {
            anchor: Anchor::TopLeft,
            ..default()
        },
        transform: Transform::from_translation(Vec3::new(
            -window.width() / 2.0,
            window.height() / 2.0,
            0.0,
        )),
        texture: assets.spinderella.clone(),
        ..default()
    })
    .insert(DatingSimEntity);
    cmds.spawn_bundle(SpriteBundle {
        sprite: Sprite {
            anchor: Anchor::TopRight,
            ..default()
        },
        transform: Transform::from_translation(Vec3::new(
            window.width() / 2.0,
            window.height() / 2.0,
            0.0,
        )),
        texture: assets.silkarella.clone(),
        ..default()
    })
    .insert(DatingSimEntity);
}

pub fn clear_dating_sim_entities(
    mut cmds: Commands<'_, '_>,
    entities: Query<Entity, With<DatingSimEntity>>,
) {
    entities
        .iter()
        .for_each(|e| cmds.entity(e).despawn_recursive());
}

pub fn dating() {
    println!("hi");
}
