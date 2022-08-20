use bevy::prelude::*;

pub struct MapAssetPlugin;

impl Plugin for MapAssetPlugin {
    fn build(&self, app: &mut App) {
        app.add_startup_system(load_assets);
    }
}

pub struct MapAssets {
    pub select: Handle<Image>,
    pub terrain: Handle<Image>,
}

fn load_assets(mut commands: Commands, assets: Res<AssetServer>) {
    commands.insert_resource(MapAssets {
        select: assets.load("image/tile/select.png"),
        terrain: assets.load("image/tile/terrain.png"),
    });
}
