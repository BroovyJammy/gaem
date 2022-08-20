use crate::prelude::*;

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
        Self(select as u32)
    }
}

// See `image/tile/unit.png`
pub enum Unit {
    None,
    Unit,
}

impl From<Unit> for TileTexture {
    fn from(unit: Unit) -> Self {
        Self(unit as u32)
    }
}

#[derive(Component)]
pub struct TerrainTile;

// See `image/tile/terrain.png`
pub enum Terrain {
    Dirt,
    Grass,
}

impl From<Terrain> for TileTexture {
    fn from(terrain: Terrain) -> Self {
        Self(terrain as u32)
    }
}
