use bevy::prelude::*;

pub struct MapAssetPlugin;

impl Plugin for MapAssetPlugin {
    fn build(&self, app: &mut App) {
        app.add_startup_system(load_assets);
    }
}

pub struct MapAssets {
    pub tiles: Handle<Image>,
}

fn load_assets(mut commands: Commands, assets: Res<AssetServer>) {
    commands.insert_resource(MapAssets {
        tiles: assets.load("image/tiles.png"),
    });
}
