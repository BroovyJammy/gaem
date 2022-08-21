use bevy::prelude::*;
use bevy_ecs_tilemap::prelude::*;
use std::collections::HashSet;

use super::UnitPos;

#[derive(Debug)]
pub enum PartDirection {
    Up = 0,
    Right = 1,
    Down = 2,
    _Left = 3,
}
#[derive(Clone, Copy, Debug)]
pub enum InsectPartKind {
    Flesh = 0,
    Head = 1,
    Legs = 2,
}

#[derive(Debug)]
pub struct InsectPart {
    pub position: (u32, u32),
    pub kind: InsectPartKind,
    pub rotation: PartDirection,
}

#[derive(Component)]
pub struct InsectBody {
    pub parts: Box<[InsectPart]>,
    pub used_tiles: HashSet<(u32, u32)>,
}

impl InsectBody {
    pub fn contains_tile(&self, insect_position: UnitPos, tile: UVec2) -> bool {
        if insect_position.x > tile.x || insect_position.y > tile.y {
            return false;
        }

        let tile = tile - insect_position.0;
        self.used_tiles.contains(&(tile.x, tile.y))
    }
}

pub fn update_insect_body_tilemap(
    mut cmds: Commands<'_, '_>,
    mut insects: Query<(Entity, &mut TileStorage, &InsectBody), Added<InsectBody>>,
) {
    for (entity, mut tilemap, body) in insects.iter_mut() {
        for part in body.parts.iter() {
            let tile_pos = TilePos::new(part.position.0, part.position.1);
            let tile_id = cmds
                .spawn_bundle(TileBundle {
                    position: tile_pos,
                    tilemap_id: TilemapId(entity),
                    texture: TileTexture(part.kind as u32 * 4 + part.rotation as u32),
                    color: TileColor(Vec4::ONE.into()),
                    ..default()
                })
                .id();
            tilemap.set(&tile_pos, Some(tile_id));
        }
    }
}
