use crate::prelude::*;
use bevy_asset_loader::prelude::*;

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
                .with_collection::<MapAssets>(),
        );
        // app.add_system_to_stage(CoreStage::Last, debug_progress.run_in_state(AppState::AssetsLoading));
    }
}

#[derive(AssetCollection)]
pub struct MapAssets {
    #[asset(key = "image.select")]
    pub select: Handle<Image>,
    #[asset(key = "image.terrain")]
    pub terrain: Handle<Image>,
    #[asset(key = "image.insect")]
    pub insect: Handle<Image>,
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

#[allow(dead_code)]
fn debug_progress(counter: Res<ProgressCounter>) {
    let progress = counter.progress();
    debug!("Progress: {}/{}", progress.done, progress.total);
    let progress = counter.progress_complete();
    debug!("Full Progress: {}/{}", progress.done, progress.total);
}
