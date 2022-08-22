use crate::{gameplay::insect_body::InsectPartKind, prelude::*};
use bevy_asset_loader::prelude::*;
use bevy_common_assets::toml::TomlAssetPlugin;

pub struct AssetsPlugin;

impl Plugin for AssetsPlugin {
    fn build(&self, app: &mut App) {
        app.add_loading_state(
            LoadingState::new(AppState::AssetsLoading)
                .continue_to_state(AppState::Game)
                .with_dynamic_collections::<StandardDynamicAssetCollection>(vec![
                    "ui.assets",
                    "game.assets",
                ])
                .with_collection::<UiAssets>()
                .with_collection::<BodyPartAssets>()
                .with_collection::<MapAssets>(),
        );
        app.add_plugin(TomlAssetPlugin::<BodyPartAsset>::new(&["bodypart.toml"]));
        // app.add_system_to_stage(CoreStage::Last, debug_progress.run_in_state(AppState::AssetsLoading));
        app.add_enter_system(AppState::Game, debug_bodyparts);
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
    pub sprite_idx: usize,
    pub connections: Vec<IVec2>,
    pub pivot: Vec2,
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
