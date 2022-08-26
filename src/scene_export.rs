use std::path::{Path, PathBuf};

use iyes_scene_tools::*;

use crate::{
    asset::HandleFromPath,
    prelude::*,
    ui::{SidebarDamage, SidebarHealth, SidebarName, SidebarSpeed},
};

/// You must add this component to the entities you wanna export via EzScene™
#[derive(Component)]
struct EzSceneExport;

/// YOUR PLAYGROUND
///
/// spawn any entities you like here
///
/// run the game, enter "EzScene™" mode, and if you like what you see,
/// press F10 to export to "ezscene.scn.ron"
///
/// do whatever you want with that ;)
///
/// GlobalTransform, Visibility, ComputedVisibility, will be ignored
/// (we have a system that will auto-add them at load time, to anything
/// with Transform)
///
/// Asset Handles will be ignored, insert a `AssetFromPath` component
/// instead (will be turned into a real handle at load time)
fn ezscene(mut commands: Commands) {
    commands
        .spawn_bundle(NodeBundle {
            style: Style {
                flex_direction: FlexDirection::RowReverse,
                align_items: AlignItems::FlexEnd,
                size: Size::new(Val::Percent(100.), Val::Percent(100.)),
                ..default()
            },
            color: Color::NONE.into(),
            ..default()
        })
        .with_children(|parent| {
            parent
                .spawn_bundle(NodeBundle {
                    style: Style {
                        flex_direction: FlexDirection::ColumnReverse,
                        align_items: AlignItems::FlexStart,
                        ..default()
                    },
                    color: Color::rgb(0.2, 0., 0.).into(),
                    ..default()
                })
                .with_children(|parent| {
                    parent
                        .spawn_bundle(TextBundle {
                            text: Text::from_section(
                                "Spider Core",
                                TextStyle {
                                    font_size: 24.,
                                    ..default()
                                },
                            ),
                            ..default()
                        })
                        .insert(SidebarName)
                        .insert(EzSceneExport);

                    let text_style = TextStyle {
                        font_size: 18.,
                        ..default()
                    };

                    parent
                        .spawn_bundle(TextBundle {
                            text: Text::from_section("Max Health: 4", text_style.clone()),
                            ..default()
                        })
                        .insert(SidebarHealth)
                        .insert(EzSceneExport);

                    parent
                        .spawn_bundle(TextBundle {
                            text: Text::from_section("Damage: 1 Adjacent", text_style.clone()),
                            ..default()
                        })
                        .insert(SidebarDamage)
                        .insert(EzSceneExport);

                    parent
                        .spawn_bundle(TextBundle {
                            text: Text::from_section("Speed: 0", text_style),
                            ..default()
                        })
                        .insert(SidebarSpeed)
                        .insert(EzSceneExport);
                })
                .insert(EzSceneExport);
        })
        .insert(EzSceneExport);
    // // have fun!
    // commands
    //     .spawn_bundle(SpriteBundle {
    //         sprite: Sprite {
    //             color: Color::PINK,
    //             ..Default::default()
    //         },
    //         ..default()
    //     })
    //     // for our image asset (no need to set the handle in SpriteBundle)
    //     .insert(HandleFromPath::<Image>::from(
    //         "image/dating/spinderella.png",
    //     ))
    //     // dont forget to mark for export
    //     .insert(EzSceneExport);
}

/// Send an event of this type to trigger a scene export
pub struct SceneExportEvent {
    pub kind: SceneExportKind,
    pub path: PathBuf,
}

/// What to include in the scene
#[allow(dead_code)]
pub enum SceneExportKind {
    Ui,
    GameLevel,
    EzScene,
}

pub struct SceneExportPlugin;

impl Plugin for SceneExportPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<Events<SceneExportEvent>>();
        app.add_system_to_stage(
            CoreStage::Last,
            export_scene_system.exclusive_system().at_end(),
        );
        app.add_enter_system(AppState::EditorEzScene, ezscene);
        app.add_system(export_on_f10.run_in_state(AppState::EditorEzScene));
        app.add_system(setup_spatial);
        app.add_system(setup_untextured_sprite);

        // uhm … more workarounds
        app.register_type::<Option<Vec2>>();
    }
}

fn export_scene_system(world: &mut World) {
    let mut export_ui = None;
    let mut export_gamelevel = None;
    let mut export_ezscene = None;

    {
        let mut events = world.resource_mut::<Events<SceneExportEvent>>();
        for ev in events.drain() {
            match ev.kind {
                SceneExportKind::Ui => {
                    export_ui = Some(ev.path);
                }
                SceneExportKind::GameLevel => {
                    export_gamelevel = Some(ev.path);
                }
                SceneExportKind::EzScene => {
                    export_ezscene = Some(ev.path);
                }
            }
        }
    }

    if let Some(path) = export_ui {
        export_scene_ui(world, &path);
    }

    if let Some(path) = export_gamelevel {
        export_scene_gamelevel(world, &path);
    }

    if let Some(path) = export_ezscene {
        export_scene_ezscene(world, &path);
    }
}

fn export_scene_ui(world: &mut World, path: &Path) {
    let mut builder = SceneBuilder::new(world);

    builder.add_from_query_filter::<
        // (
        //     (Option<&Parent>, Option<&Children>),
        //     (&Style, Option<&UiColor>, Option<&UiImage>, Option<&Text>, Option<&Button>),
        // ),
        With<Node>,
    >();
    builder.ignore_components::<(
        &Transform,
        &GlobalTransform,
        &Visibility,
        &ComputedVisibility,
        &Node,
        &CalculatedSize,
        &Text,
    )>();

    if let Err(e) = builder.export_to_file(path) {
        error!("UI Scene export to {:?} failed: {}", path, e);
    }
}

fn export_scene_gamelevel(world: &mut World, path: &Path) {
    let builder = SceneBuilder::new(world);

    if let Err(e) = builder.export_to_file(path) {
        error!("Game Level Scene export to {:?} failed: {:#}", path, e);
    }
}

fn export_scene_ezscene(world: &mut World, path: &Path) {
    let mut builder = SceneBuilder::new(world);

    builder.add_from_query_filter::<With<EzSceneExport>>();
    builder.ignore_components::<(
        &GlobalTransform,
        &Visibility,
        &ComputedVisibility,
        &Handle<Image>,
        &Handle<Font>,
        &Handle<DynamicScene>,
    )>();

    if let Err(e) = builder.export_to_file(path) {
        error!("EzScene™ Scene export to {:?} failed: {}", path, e);
    }
    info!("EzScene™ exported successfully to {:?}", path);
}

fn export_on_f10(input: Res<Input<KeyCode>>, mut evw: EventWriter<SceneExportEvent>) {
    if input.just_pressed(KeyCode::F10) {
        evw.send(SceneExportEvent {
            kind: SceneExportKind::EzScene,
            path: "ezscene.scn.ron".into(),
        });
    }
}

fn setup_spatial(
    mut commands: Commands,
    q: Query<(Entity, &Transform), (Added<Transform>, Without<GlobalTransform>)>,
) {
    for (e, xf) in q.iter() {
        commands.entity(e).insert_bundle(SpatialBundle {
            transform: *xf,
            ..Default::default()
        });
    }
}

fn setup_untextured_sprite(
    mut commands: Commands,
    q: Query<Entity, (Added<Sprite>, Without<Handle<Image>>)>,
) {
    for e in q.iter() {
        commands
            .entity(e)
            .insert(bevy::render::texture::DEFAULT_IMAGE_HANDLE.typed::<Image>());
    }
}
