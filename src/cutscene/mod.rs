use bevy::{
    render::camera::ScalingMode,
    utils::{FloatOrd, Instant},
};

use crate::{
    asset::{CutsceneAssets, UiScenes},
    gameplay::Action,
    prelude::*,
    ui::{TextProps, TextPurpose},
};

pub struct CutscenePlugin;

impl Plugin for CutscenePlugin {
    fn build(&self, app: &mut App) {
        app.add_system_to_stage("fuckstages", cutscene_driver.run_in_state(AppState::PlayCutscene));
        app.add_system(update_dialogue_box.run_in_state(AppState::PlayCutscene));
        app.add_enter_system(AppState::PlayCutscene, init_cutscene);
        app.add_enter_system(AppState::PlayCutscene, setup_dialogue_box);
        app.add_enter_system(AppState::PlayCutscene, camera_projection_enter);
        app.add_exit_system(AppState::PlayCutscene, camera_projection_exit);
        app.add_exit_system(AppState::PlayCutscene, cleanup_cutscene);
        app.add_exit_system(AppState::PlayCutscene, despawn_with::<DialogueBox>);
        app.add_exit_system(
            AppState::PlayCutscene,
            remove_resource::<CutscenePlayerState>,
        );
        app.register_type::<DialogueBox>();
    }
}

#[derive(bevy::reflect::TypeUuid, serde::Deserialize)]
#[uuid = "f2ff9826-32c4-4a69-95df-2428b9a6e6b3"]
pub struct CutsceneMetaAsset {
    pub title: String,
    pub dialogue: Vec<DialogueEntry>,
    #[serde(default)]
    pub spawn_scene: Vec<SpawnSceneEntry>,
}

#[derive(serde::Deserialize)]
pub struct DialogueEntry {
    #[serde(default)]
    pub transition: DialogueNext,
    #[serde(default)]
    pub mode: DialogueMode,
    pub name: String,
    pub text: String,
}

/// Where to go after the current dialogue entry?
#[derive(serde::Deserialize, Default)]
pub enum DialogueNext {
    /// Go to the next dialogue entry
    #[default]
    Auto,
    /// Go to another dialogue entry in the same cutscene
    Entry(usize),
    /// End the cutscene and go into another App State
    AppState(AppState),
    /// End the cutscene and go into another cutscene
    Cutscene(String),
}

/// How to trigger the transition?
#[derive(Default, serde::Deserialize)]
pub enum DialogueMode {
    #[default]
    WaitForever,
    WaitSeconds(f32),
}

#[derive(serde::Deserialize)]
pub struct SpawnSceneEntry {
    /// trigger on this entry
    spawn_on_dialogue: usize,
    /// despawn on this entry
    despawn_on_dialogue: Option<usize>,
    /// at this time after dialogue entry
    delay: Option<f32>,
    /// despawn this many seconds after spawn
    duration: Option<f32>,
    name: String,
}

/// Resource describing the currently active cutscene
pub struct CurrentCutscene {
    title: String,
    handle: Option<Handle<CutsceneMetaAsset>>,
}

impl CurrentCutscene {
    pub fn new(title: &str) -> Self {
        Self {
            title: title.into(),
            handle: None,
        }
    }
}

struct CutscenePlayerState {
    start: Instant,
    current_dialogue: usize,
    next_scene_spawn: Option<usize>,
    dialogue_start_time: f32,
    active_scenes: HashMap<Handle<DynamicScene>, Option<f32>>,
    needs_init: bool,
}

fn init_cutscene(
    time: Res<Time>,
    mut commands: Commands,
    collection: Res<CutsceneAssets>,
    mut assets: ResMut<Assets<CutsceneMetaAsset>>,
    mut current: ResMut<CurrentCutscene>,
    mut camera: Query<&mut Transform, With<Camera>>,
) {
    let mut camera_trans = camera.single_mut();
    camera_trans.translation.x = 0.;
    camera_trans.translation.y = 0.;
    camera_trans.translation.z = 900.;

    current.handle = Some(find_cutscene_by_title(
        &*collection,
        &*assets,
        &current.title,
    ));

    // sort scene spawn/despawn events by time and then by dialogue index
    let meta = assets.get_mut(current.handle.as_ref().unwrap()).unwrap();
    meta.spawn_scene
        .sort_by_key(|entry| entry.delay.map(|x| FloatOrd(x)));
    meta.spawn_scene
        .sort_by_key(|entry| entry.spawn_on_dialogue);

    let player = CutscenePlayerState {
        start: time.startup() + time.time_since_startup(),
        next_scene_spawn: None,
        current_dialogue: 0,
        dialogue_start_time: 0.0,
        active_scenes: Default::default(),
        needs_init: true,
    };

    commands.insert_resource(player);
    commands.insert_resource(CutsceneText(
        meta.dialogue[0].name.clone(),
        meta.dialogue[0].text.clone(),
        false,
    ));
}

fn cleanup_cutscene(
    mut scenespawner: ResMut<SceneSpawner>,
    mut player: ResMut<CutscenePlayerState>,
    ui_scenes: Res<UiScenes>,
) {
    scenespawner.despawn(ui_scenes.dialogue_box.clone());
    for (handle, _) in player.active_scenes.drain() {
        scenespawner.despawn(handle);
    }
}

fn cutscene_driver(
    time: Res<Time>,
    mut commands: Commands,
    current: Res<CurrentCutscene>,
    mut ctext: ResMut<CutsceneText>,
    assets: Res<Assets<CutsceneMetaAsset>>,
    mut player: ResMut<CutscenePlayerState>,
    mut scenespawner: ResMut<SceneSpawner>,
    ass: Res<AssetServer>,
    actioner: Query<&ActionState<Action>>,
) {
    let now = ((time.startup() + time.time_since_startup()) - player.start).as_secs_f32();
    let meta = assets.get(current.handle.as_ref().unwrap()).unwrap();

    if let Some(dialogue_entry) = meta.dialogue.get(player.current_dialogue) {
        if player.needs_init {
            player.needs_init = false;
            player.next_scene_spawn = meta
                .spawn_scene
                .iter()
                .position(|entry| entry.spawn_on_dialogue == player.current_dialogue);

            while let Some(spawn_scene) = player
                .next_scene_spawn
                .and_then(|i| meta.spawn_scene.get(i))
            {
                if spawn_scene.spawn_on_dialogue != player.current_dialogue {
                    break;
                }
                if spawn_scene.delay.is_none() {
                    let handle = get_scene_by_name(&ass, &spawn_scene.name);
                    player.next_scene_spawn = player.next_scene_spawn.map(|x| x + 1);
                    player
                        .active_scenes
                        .insert(handle.clone(), spawn_scene.duration.map(|x| now + x));
                    scenespawner.spawn_dynamic(handle);
                }
            }
        }
        let mut transition = actioner.single().just_pressed(Action::Select);
        match dialogue_entry.mode {
            DialogueMode::WaitForever => {}
            DialogueMode::WaitSeconds(secs) => {
                if now > player.dialogue_start_time + secs {
                    transition = true;
                    player.dialogue_start_time = now;
                }
            }
        }
        if transition {
            for entry in meta.spawn_scene.iter() {
                if entry.despawn_on_dialogue != Some(player.current_dialogue) {
                    continue;
                }
                let handle = get_scene_by_name(&ass, &entry.name);
                scenespawner.despawn(handle);
            }

            match &dialogue_entry.transition {
                DialogueNext::Auto => {
                    player.current_dialogue += 1;
                }
                DialogueNext::Entry(n) => {
                    player.current_dialogue = *n;
                }
                DialogueNext::AppState(state) => {
                    commands.insert_resource(NextState(*state));
                }
                DialogueNext::Cutscene(cutscene) => {
                    commands.insert_resource(NextState(AppState::PlayCutscene));
                    commands.insert_resource(CurrentCutscene::new(cutscene));
                }
            }

            ctext.0 = meta.dialogue[player.current_dialogue].name.clone();
            ctext.1 = meta.dialogue[player.current_dialogue].text.clone();
            ctext.2 = false;

            player.next_scene_spawn = meta
                .spawn_scene
                .iter()
                .position(|entry| entry.spawn_on_dialogue == player.current_dialogue);

            while let Some(spawn_scene) = player
                .next_scene_spawn
                .and_then(|i| meta.spawn_scene.get(i))
            {
                if spawn_scene.delay.is_none() {
                    let handle = get_scene_by_name(&ass, &spawn_scene.name);
                    player.next_scene_spawn = player.next_scene_spawn.map(|x| x + 1);
                    player
                        .active_scenes
                        .insert(handle.clone(), spawn_scene.duration.map(|x| now + x));
                    scenespawner.spawn_dynamic(handle);
                }
            }
        }
    }

    player
        .active_scenes
        .drain_filter(|_, end| end.map(|x| now > x).unwrap_or(false))
        .for_each(|(handle, _)| scenespawner.despawn(handle));

    while let Some(spawn_scene) = player
        .next_scene_spawn
        .and_then(|i| meta.spawn_scene.get(i))
    {
        if spawn_scene.spawn_on_dialogue != player.current_dialogue {
            break;
        }
        if let Some(delay) = spawn_scene.delay {
            if now > player.dialogue_start_time + delay {
                let handle = get_scene_by_name(&ass, &spawn_scene.name);
                player.next_scene_spawn = player.next_scene_spawn.map(|x| x + 1);
                player
                    .active_scenes
                    .insert(handle.clone(), spawn_scene.duration.map(|x| now + x));
                scenespawner.spawn_dynamic(handle);
            }
        }
    }
}

struct CutsceneText(String, String, bool);

fn get_scene_by_name(ass: &AssetServer, name: &str) -> Handle<DynamicScene> {
    let path = format!("scene/cutscenes/{}.scn.ron", name);
    ass.load(&path)
}

fn find_cutscene_by_title(
    collection: &CutsceneAssets,
    assets: &Assets<CutsceneMetaAsset>,
    title: &str,
) -> Handle<CutsceneMetaAsset> {
    for handle in collection.meta.iter() {
        let ass = assets.get(handle).unwrap();
        if ass.title == title {
            return handle.clone();
        }
    }
    panic!("Unknown cutscene title {:?}", title);
}

fn setup_dialogue_box(mut scene_spawner: ResMut<SceneSpawner>, ui_scenes: Res<UiScenes>) {
    scene_spawner.spawn_dynamic(ui_scenes.dialogue_box.clone());
}

#[derive(Component, Default, Reflect, FromReflect)]
#[reflect(Component)]
struct DialogueBox;

fn update_dialogue_box(
    mut commands: Commands,
    mut ctext: ResMut<CutsceneText>,
    q: Query<Entity, With<DialogueBox>>,
) {
    if ctext.2 {
        return;
    }
    if let Ok(e) = q.get_single() {
        ctext.2 = true;
        commands.entity(e).despawn_descendants();
        let name = commands
            .spawn_bundle(NodeBundle {
                color: UiColor(Color::rgb(0.11, 0.13, 0.12)),
                style: Style {
                    flex_wrap: FlexWrap::Wrap,
                    margin: UiRect::all(Val::Px(8.0)),
                    ..Default::default()
                },
                ..Default::default()
            })
            .with_children(|p| {
                p.spawn_bundle(TextBundle {
                    ..Default::default()
                })
                .insert(TextProps {
                    value: ctext.0.clone(),
                    purpose: TextPurpose::Heading,
                });
            })
            .id();
        let text = commands
            .spawn_bundle(NodeBundle {
                color: UiColor(Color::rgb(0.11, 0.13, 0.12)),
                style: Style {
                    flex_direction: FlexDirection::Row,
                    align_content: AlignContent::FlexStart,
                    align_items: AlignItems::FlexStart,
                    justify_content: JustifyContent::FlexStart,
                    flex_wrap: FlexWrap::WrapReverse,
                    margin: UiRect::all(Val::Px(8.0)),
                    ..Default::default()
                },
                ..Default::default()
            })
            .with_children(|p| {
                for word in ctext.1.split_ascii_whitespace() {
                    p.spawn_bundle(TextBundle {
                        ..Default::default()
                    })
                    .insert(TextProps {
                        value: format!("{} ", word),
                        purpose: TextPurpose::Dialogue,
                    });
                }
            })
            .id();
        commands.entity(e).push_children(&[name, text]);
    }
}

fn camera_projection_enter(
    // assume there is only one camera
    mut q: Query<&mut OrthographicProjection>,
) {
    let mut proj = q.single_mut();
    proj.scaling_mode = ScalingMode::Auto { min_height: 600.0, min_width: 800.0 };
    // proj.left = -400.;
    // proj.right = 400.;
    // proj.top = 300.;
    // proj.bottom = -300.;
}

fn camera_projection_exit(
    // assume there is only one camera
    mut q: Query<&mut OrthographicProjection>,
) {
    let mut proj = q.single_mut();
    proj.scaling_mode = ScalingMode::WindowSize;
}
