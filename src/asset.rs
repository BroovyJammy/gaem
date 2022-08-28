use std::marker::PhantomData;

use crate::{
    cutscene::CutsceneMetaAsset,
    gameplay::{insect_body::InsectPartKind, map::TerrainKind},
    prelude::*,
};
use bevy::asset::Asset;
use bevy_asset_loader::prelude::*;
use bevy_common_assets::toml::TomlAssetPlugin;

pub struct AssetsPlugin;

impl Plugin for AssetsPlugin {
    fn build(&self, app: &mut App) {
        app.add_loading_state(
            LoadingState::new(AppState::AssetsLoading)
                .continue_to_state(AppState::MainMenu)
                .with_dynamic_collections::<StandardDynamicAssetCollection>(vec![
                    // put UI-related things here
                    // (fonts, images, sounds, scenes)
                    "ui.assets",
                    // put gameplay-related things here
                    // (spritesheets, bodyparts, sounds, etc)
                    "game.assets",
                    // put cutscene-related things here
                    // (images, metadata, scenes, etc)
                    "cutscene.assets",
                ])
                .with_collection::<UiAssets>()
                .with_collection::<UiScenes>()
                .with_collection::<BodyPartAssets>()
                .with_collection::<TerrainAssets>()
                .with_collection::<CutsceneAssets>()
                .with_collection::<MapAssets>()
                .with_collection::<AudioAssets>(),
        );
        app.add_plugin(TomlAssetPlugin::<BodyPartAsset>::new(&["bodypart.toml"]));
        app.add_plugin(TomlAssetPlugin::<TerrainAsset>::new(&["terrain.toml"]));
        app.add_plugin(TomlAssetPlugin::<CutsceneMetaAsset>::new(&[
            "cutscene.toml",
        ]));
        // app.add_system_to_stage(CoreStage::Last, debug_progress.run_in_state(AppState::AssetsLoading));
        // app.add_enter_system(AppState::Game, debug_bodyparts);
        app.add_startup_system(enable_hot_reloading);

        // workaround cuz scenes dont support asset handles properly
        app.register_type::<HandleFromPath<Image>>();
        app.add_system_to_stage("fuckstages", setup_handle_from_path::<Image>);
        app.register_type::<HandleFromPath<Font>>();
        app.add_system_to_stage("fuckstages", setup_handle_from_path::<Font>);
        app.register_type::<HandleFromPath<DynamicScene>>();
        app.add_system_to_stage("fuckstages", setup_handle_from_path::<DynamicScene>);
        // … add others if needed
    }
}

#[derive(AssetCollection)]
pub struct MapAssets {
    #[asset(key = "image.select")]
    pub select: Handle<Image>,
    #[asset(key = "image.terrain")]
    pub terrain: Handle<Image>,
    #[asset(key = "image.insect")]
    pub insect: Handle<TextureAtlas>,
}

#[derive(AssetCollection)]
pub struct UiAssets {
    #[asset(key = "font.regular")]
    pub font_regular: Handle<Font>,
    #[asset(key = "font.bold")]
    pub font_bold: Handle<Font>,
    #[asset(key = "font.light")]
    pub font_light: Handle<Font>,
    #[asset(key = "dating.spinderella")]
    pub spinderella: Handle<Image>,
    #[asset(key = "dating.silkarella")]
    pub silkarella: Handle<Image>,
}

#[derive(AssetCollection)]
pub struct UiScenes {
    #[asset(key = "scene.main_menu")]
    pub main_menu: Handle<DynamicScene>,
    #[asset(key = "scene.dialogue_box")]
    pub dialogue_box: Handle<DynamicScene>,
}

#[derive(AssetCollection)]
pub struct AudioAssets {
    #[asset(key = "sound.music")]
    pub music: Handle<AudioSource>,
}

/// This will be available as a resource
///
/// Contains all the body part descriptors loaded from asset files
#[derive(Deref)]
pub struct BodyParts(pub Vec<BodyPartDescriptor>);

impl std::ops::Index<InsectPartKind> for BodyParts {
    type Output = BodyPartDescriptor;
    fn index(&self, index: InsectPartKind) -> &Self::Output {
        &self.0[index.0]
    }
}

#[derive(Debug, Clone, serde::Deserialize)]
pub struct BodyPartDescriptor {
    pub name: String,
    // Name that can be shown to players
    pub pub_name: String,
    pub max_health: u32,
    pub move_bonus: u32,
    pub damage: u32,
    // An insect needs at least one base part, or it vanishes
    // This is to prevent floating heads, wings, etc
    pub base: bool,
    pub sprite_idx: usize,
    pub connections: Vec<IVec2>,
    pub pivot: Vec2,
}

#[derive(Deref)]
pub struct Terrain(pub Vec<TerrainDescriptor>);

impl std::ops::Index<TerrainKind> for Terrain {
    type Output = TerrainDescriptor;
    fn index(&self, index: TerrainKind) -> &Self::Output {
        &self.0[index.0]
    }
}

#[derive(Debug, Clone, serde::Deserialize)]
pub struct TerrainDescriptor {
    pub name: String,
    pub sprite_idx: usize,
    pub wall: bool,
}

/// This can be added to things to be loaded from scenes
/// (bevy cannot handle handles in scenes yet ;) )
/// At runtime, will be replaced with handle
#[derive(Component, Clone, Reflect, FromReflect)]
#[reflect(Component)]
pub struct HandleFromPath<T: Asset> {
    pub path: String,
    #[reflect(ignore)]
    pub _pd: PhantomData<T>,
}

// [INTERNAL THINGS BELOW] //

/// internal thingy to load all the asset files
/// and accumulate them into a BodyParts resource
#[derive(AssetCollection)]
struct BodyPartAssets {
    #[asset(key = "meta.bodyparts", collection(typed))]
    #[allow(dead_code)]
    handles: Vec<Handle<BodyPartAsset>>,
    #[allow(dead_code)]
    all: BodyPartsMarker,
}

struct BodyPartsMarker;

#[derive(bevy::reflect::TypeUuid, serde::Deserialize)]
#[uuid = "78a56857-57f8-4f05-b639-c2c3b7a00085"]
struct BodyPartAsset {
    bodypart: Vec<BodyPartDescriptor>,
}

impl FromWorld for BodyPartsMarker {
    fn from_world(world: &mut World) -> Self {
        let mut all = Vec::new();
        {
            let assets = world.resource::<Assets<BodyPartAsset>>();
            for (_, asset) in assets.iter() {
                for desc in asset.bodypart.iter() {
                    all.push(desc.clone());
                }
            }
        }
        world.insert_resource(BodyParts(all));
        BodyPartsMarker
    }
}

#[derive(AssetCollection)]
struct TerrainAssets {
    #[asset(key = "meta.terrain", collection(typed))]
    #[allow(dead_code)]
    handles: Vec<Handle<TerrainAsset>>,
    #[allow(dead_code)]
    all: TerrainMarker,
}

struct TerrainMarker;

#[derive(bevy::reflect::TypeUuid, serde::Deserialize)]
#[uuid = "25dce551-aa36-4c9e-b9a8-5f8dfe0df239"]
struct TerrainAsset {
    terrain: Vec<TerrainDescriptor>,
}

impl FromWorld for TerrainMarker {
    fn from_world(world: &mut World) -> Self {
        let mut all = Vec::new();
        {
            let assets = world.resource::<Assets<TerrainAsset>>();
            for (_, asset) in assets.iter() {
                for desc in asset.terrain.iter() {
                    all.push(desc.clone());
                }
            }
        }
        world.insert_resource(Terrain(all));
        TerrainMarker
    }
}

#[derive(AssetCollection)]
pub struct CutsceneAssets {
    #[asset(key = "cutscenes.meta", collection(typed))]
    pub meta: Vec<Handle<CutsceneMetaAsset>>,
    #[asset(key = "cutscenes.scenes", collection(typed))]
    pub scene: Vec<Handle<DynamicScene>>,
}

#[allow(dead_code)]
fn enable_hot_reloading(ass: Res<AssetServer>) {
    ass.watch_for_changes().ok();
}

#[allow(dead_code)]
fn debug_progress(counter: Res<ProgressCounter>) {
    let progress = counter.progress();
    debug!("Progress: {}/{}", progress.done, progress.total);
    let progress = counter.progress_complete();
    debug!("Full Progress: {}/{}", progress.done, progress.total);
}

#[allow(dead_code)]
fn debug_bodyparts(bp: Res<BodyParts>) {
    dbg!(&bp.0);
}

impl<T: Asset> Default for HandleFromPath<T> {
    fn default() -> Self {
        Self {
            path: "".into(),
            _pd: PhantomData,
        }
    }
}

impl<T: Asset> From<&str> for HandleFromPath<T> {
    fn from(str: &str) -> Self {
        Self {
            path: str.into(),
            _pd: PhantomData,
        }
    }
}

fn setup_handle_from_path<T: Asset>(
    mut commands: Commands,
    q: Query<(Entity, &HandleFromPath<T>), Changed<HandleFromPath<T>>>,
    ass: Res<AssetServer>,
) {
    for (e, hfp) in q.iter() {
        commands.entity(e).insert(ass.load::<T, _>(&hfp.path));
    }
}
