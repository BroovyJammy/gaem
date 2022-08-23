use crate::{
    gameplay::{insect_body::InsectPartKind, map::TerrainKind},
    prelude::*,
};
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
                .with_collection::<MapAssets>(),
        );
        app.add_plugin(TomlAssetPlugin::<BodyPartAsset>::new(&["bodypart.toml"]));
        app.add_plugin(TomlAssetPlugin::<TerrainAsset>::new(&["terrain.toml"]));
        app.add_plugin(TomlAssetPlugin::<CutsceneMetaAsset>::new(&[
            "cutscene.toml",
        ]));
        // app.add_system_to_stage(CoreStage::Last, debug_progress.run_in_state(AppState::AssetsLoading));
        // app.add_enter_system(AppState::Game, debug_bodyparts);
        app.add_startup_system(enable_hot_reloading);
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

#[derive(bevy::reflect::TypeUuid, serde::Deserialize)]
#[uuid = "f2ff9826-32c4-4a69-95df-2428b9a6e6b3"]
pub struct CutsceneMetaAsset {
    pub title: String,
    pub dialogue: Vec<DialogueEntry>,
    // TODO (boxy) add any other info you want for each cutscene here
}

#[derive(serde::Deserialize)]
pub struct DialogueEntry {
    pub name: String,
    pub text: String,
}

#[derive(AssetCollection)]
pub struct CutsceneAssets {
    #[asset(key = "meta.cutscenes", collection(typed))]
    pub meta: Vec<Handle<CutsceneMetaAsset>>,
    // TODO (ida) also add any bevy scenes to use for cutscenes here
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
