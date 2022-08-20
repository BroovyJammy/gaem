use bevy::prelude::*;
use bevy_ecs_tilemap::prelude::*;
use std::collections::HashSet;

#[derive(Debug)]
pub enum PartDirection {
    Up,
    _Down,
    _Left,
    _Right,
}
#[derive(Debug)]
pub enum InsectPartKind {
    Flesh,
    _Head,
    _Legs,
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
                    texture: TileTexture(part.kind as u32),
                    color: TileColor(Vec4::ONE.into()),
                    ..default()
                })
                .id();
            tilemap.set(&tile_pos, Some(tile_id));
        }
    }
}
