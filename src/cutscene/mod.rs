use bevy::utils::FloatOrd;

use crate::{prelude::*, asset::{CutsceneAssets, UiScenes}, scene_export};

pub struct CutscenePlugin;

impl Plugin for CutscenePlugin {
    fn build(&self, app: &mut App) {
        app.add_system(
            cutscene_driver
                .run_in_state(AppState::PlayCutscene)
        );
        app.add_enter_system(AppState::PlayCutscene, init_cutscene);
        app.add_enter_system(AppState::PlayCutscene, setup_dialogue_box);
        app.add_exit_system(AppState::PlayCutscene, cleanup_cutscene);
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
    #[serde(default)]
    pub despawn_scene: Vec<DespawnSceneEntry>,
}

#[derive(serde::Deserialize)]
pub struct DialogueEntry {
    #[serde(default)]
    pub transition: DialogueNext,
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
#[derive(serde::Deserialize)]
pub enum DialogueMode {
    WaitSeconds(f32),
}

#[derive(serde::Deserialize)]
pub struct SpawnSceneEntry {
    t_sec: f32,
    name: String,
}

#[derive(serde::Deserialize)]
pub struct DespawnSceneEntry {
    t_sec: f32,
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
    next_scene_spawn: usize,
    next_scene_despawn: usize,
    current_dialogue: usize,
    dialogue_start_time: f32,
    active_scenes: HashSet<Handle<DynamicScene>>,
}

fn init_cutscene(
    time: Res<Time>,
    mut commands: Commands,
    collection: Res<CutsceneAssets>,
    mut assets: ResMut<Assets<CutsceneMetaAsset>>,
    mut current: ResMut<CurrentCutscene>,
) {
    current.handle = Some(find_cutscene_by_title(&*collection, &*assets, &current.title));

    // sort scene spawn/despawn events by time
    let meta = assets.get_mut(current.handle.as_ref().unwrap()).unwrap();
    meta.spawn_scene.sort_by_key(|entry| FloatOrd(entry.t_sec));
    meta.despawn_scene.sort_by_key(|entry| FloatOrd(entry.t_sec));

    let player = CutscenePlayerState {
        start: time.startup() + time.time_since_startup(),
        next_scene_spawn: 0,
        next_scene_despawn: 0,
        current_dialogue: 0,
        dialogue_start_time: 0.0,
        active_scenes: Default::default(),
    };

    commands.insert_resource(player);
}

fn cleanup_cutscene(
    mut commands: Commands,
    mut scenespawner: ResMut<SceneSpawner>,
    mut player: ResMut<CutscenePlayerState>,
    ui_scenes: Res<UiScenes>,
) {
    scenespawner.despawn(ui_scenes.dialogue_box.clone());
    for handle in player.active_scenes.drain() {
        scenespawner.despawn(handle);
    }
    commands.remove_resource::<CutscenePlayerState>();
}

fn cutscene_driver(
    time: Res<Time>,
    mut commands: Commands,
    current: Res<CurrentCutscene>,
    assets: Res<Assets<CutsceneMetaAsset>>,
    mut player: ResMut<CutscenePlayerState>,
    mut scenespawner: ResMut<SceneSpawner>,
    ass: Res<AssetServer>,
) {
    let now = ((time.startup() + time.time_since_startup()) - player.start).as_secs_f32();
    let meta = assets.get(current.handle.as_ref().unwrap()).unwrap();

    if let Some(dialogue_entry) = meta.dialogue.get(player.current_dialogue) {
        let mut transition = false;
        match dialogue_entry.mode {
            DialogueMode::WaitSeconds(secs) => {
                if now > player.dialogue_start_time + secs {
                    transition = true;
                    player.dialogue_start_time = now;
                }
            }
        }
        if transition {
            match &dialogue_entry.transition {
                DialogueNext::Auto => {
                    player.current_dialogue += 1;
                }
                DialogueNext::Entry(n) => {
                    player.current_dialogue = *n;
                }
                DialogueNext::AppState(state) => {
                    commands.insert_resource(NextState(state.clone()));
                }
                DialogueNext::Cutscene(cutscene) => {
                    commands.insert_resource(NextState(AppState::PlayCutscene));
                    commands.insert_resource(CurrentCutscene::new(&cutscene));
                }
            }
        }
    }

    loop {
        if let Some(despawn_scene) = meta.despawn_scene.get(player.next_scene_despawn) {
            if now > despawn_scene.t_sec {
                let handle = get_scene_by_name(&ass, &despawn_scene.name);
                player.next_scene_despawn += 1;
                player.active_scenes.remove(&handle);
                scenespawner.despawn(handle);
                continue;
            }
        }
        break;
    }

    loop {
        if let Some(spawn_scene) = meta.spawn_scene.get(player.next_scene_spawn) {
            if now > spawn_scene.t_sec {
                let handle = get_scene_by_name(&ass, &spawn_scene.name);
                player.next_scene_spawn += 1;
                player.active_scenes.insert(handle.clone());
                scenespawner.spawn_dynamic(handle);
                continue;
            }
        }
        break;
    }
}

fn get_scene_by_name(ass: &AssetServer, name: &str) -> Handle<DynamicScene> {
    let path = format!("scene/cutscenes/{}.scn.ron", name);
    ass.load(&path)
}

fn find_cutscene_by_title(
    collection: &CutsceneAssets,
    assets: &Assets<CutsceneMetaAsset>,
    title: &str
) -> Handle<CutsceneMetaAsset> {
    for handle in collection.meta.iter() {
        let ass = assets.get(handle).unwrap();
        if ass.title == title {
            return handle.clone();
        }
    }
    panic!("Unknown cutscene title {:?}", title);
}

fn setup_dialogue_box(
    mut scene_spawner: ResMut<SceneSpawner>,
    ui_scenes: Res<UiScenes>,
) {
    scene_spawner.spawn_dynamic(ui_scenes.dialogue_box.clone());
}

#[derive(Component, Default, Reflect, FromReflect)]
#[reflect(Component)]
struct DialogueBox;

fn update_dialogue_box(
    mut commands: Commands,
    player: Res<CutscenePlayerState>,
    q: Query<Entity, With<DialogueBox>>,
) {
    if !player.is_changed() {
        return;
    }
}

