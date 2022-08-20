use bevy::prelude::*;
use bevy_ecs_tilemap::prelude::*;

#[derive(Component)]
pub struct SelectTile;

// Should follow the order of `image/tile/select.png`. Converts to `TileTexture` with `as u32`.
#[derive(Default)]
pub enum Select {
    Inactive,
    #[default]
    Active,
}

impl From<Select> for TileTexture {
    fn from(select: Select) -> Self {
        TileTexture(select as u32)
    }
}

#[derive(Component)]
pub struct TerrainTile;

// Should follow the order of `image/tile/terrain.png`. Converts to `TileTexture` with `as u32`.
pub enum Terrain {
    Dirt,
    Grass,
}

impl From<Terrain> for TileTexture {
    fn from(terrain: Terrain) -> Self {
        TileTexture(terrain as u32)
    }
}