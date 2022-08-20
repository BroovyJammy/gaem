use crate::prelude::*;
use bevy_asset_loader::prelude::*;

pub struct AssetsPlugin;

impl Plugin for AssetsPlugin {
    fn build(&self, app: &mut App) {
        app.add_loading_state(
            LoadingState::new(AppState::AssetsLoading)
                .with_dynamic_collections::<StandardDynamicAssetCollection>(vec![
                    "ui.assets",
                    "game.assets",
                ])
                .with_collection::<UiAssets>()
                .with_collection::<MapAssets>()
        );
        // workaround for progress tracking bug
        app.add_system(
            iyes_progress::dummy_system_wait_frames::<5>
                .track_progress()
                .run_in_state(AppState::AssetsLoading)
        );
        // app.add_system_to_stage(CoreStage::Last, debug_progress.run_in_state(AppState::AssetsLoading));
    }
}

#[derive(AssetCollection)]
pub struct MapAssets {
    #[asset(key = "image.select")]
    pub select: Handle<Image>,
    #[asset(key = "image.unit")]
    pub unit: Handle<Image>,
    #[asset(key = "image.terrain")]
    pub terrain: Handle<Image>,
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
fn debug_progress(
    counter: Res<ProgressCounter>,
) {
    let progress = counter.progress();
    debug!("Progress: {}/{}", progress.done, progress.total);
    let progress = counter.progress_complete();
    debug!("Full Progress: {}/{}", progress.done, progress.total);
}
