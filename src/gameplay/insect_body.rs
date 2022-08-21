use bevy::prelude::*;
use bevy_ecs_tilemap::prelude::*;
use rand::Rng;
use std::collections::HashSet;

use super::{Team, UnitPos};

#[derive(Clone, Copy, Debug)]
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

impl InsectPartKind {
    fn move_bonus(self) -> u32 {
        match self {
            Self::Legs => 2,
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct InsectPart {
    pub position: (u32, u32),
    pub kind: InsectPartKind,
    pub rotation: PartDirection,
}
impl InsectPart {
    pub fn new(pos: (u32, u32), kind: InsectPartKind, rot: PartDirection) -> Self {
        Self {
            position: pos,
            kind,
            rotation: rot,
        }
    }
}

#[derive(Component, Clone)]
pub struct InsectBody {
    pub parts: Box<[InsectPart]>,
    pub used_tiles: HashSet<(u32, u32)>,
}

impl InsectBody {
    pub fn new(parts: Vec<InsectPart>) -> Self {
        let parts = parts.into_boxed_slice();
        let used_tiles = parts.iter().map(|part| part.position).collect();
        Self { parts, used_tiles }
    }

    pub fn contains_tile(&self, insect_position: UnitPos, tile: UVec2) -> bool {
        if insect_position.x > tile.x || insect_position.y > tile.y {
            return false;
        }

        let tile = tile - insect_position.0;
        self.used_tiles.contains(&(tile.x, tile.y))
    }

    pub fn _intersects(
        &self,
        insect_position: UnitPos,
        other_insect: &InsectBody,
        other_insect_position: UnitPos,
    ) -> bool {
        other_insect.used_tiles.iter().any(|(x, y)| {
            let pos = UVec2::new(x + other_insect_position.x, y + other_insect_position.y);
            self.contains_tile(insect_position, pos)
        })
    }

    pub fn move_speed(&self) -> u32 {
        3 + self
            .parts
            .iter()
            .map(|part| part.kind.move_bonus())
            .sum::<u32>()
    }

    pub fn get_part(&self, local_tile: (u32, u32)) -> Option<&InsectPart> {
        self.parts.iter().find(|part| part.position == local_tile)
    }

    /// # Panics
    /// Panics if `local_tile` does not have any insect parts on it
    pub fn adjacent_parts(
        &self,
        local_tile: (u32, u32),
    ) -> impl Iterator<Item = Option<&InsectPart>> {
        assert!(self.used_tiles.contains(&local_tile));

        [
            (-1, 0),
            (-1, 1),
            (0, 1),
            (-1, 0),
            (1, 0),
            (1, -1),
            (1, 1),
            (-1, -1),
        ]
        .into_iter()
        .map(move |offset| {
            let adj_tile = (
                (local_tile.0 as i32 + offset.0).try_into().ok()?,
                (local_tile.1 as i32 + offset.1).try_into().ok()?,
            );
            self.get_part(adj_tile)
        })
    }
}

pub fn update_insect_body_tilemap(
    mut cmds: Commands<'_, '_>,
    mut insects: Query<(Entity, &mut TileStorage, &InsectBody, &Team), Added<InsectBody>>,
) {
    for (entity, mut tilemap, body, team) in insects.iter_mut() {
        for part in body.parts.iter() {
            let tile_pos = TilePos::new(part.position.0, part.position.1);
            let tile_id = cmds
                .spawn_bundle(TileBundle {
                    position: tile_pos,
                    tilemap_id: TilemapId(entity),
                    texture: TileTexture(part.kind as u32 * 4 + part.rotation as u32),
                    color: TileColor(team.color()),
                    ..default()
                })
                .id();
            tilemap.set(&tile_pos, Some(tile_id));
        }
    }
}

pub fn merge_insect_bodies(a: &InsectBody, b: &InsectBody) -> InsectBody {
    let filter = |body: &InsectBody, part: &&InsectPart| {
        match part.kind {
            InsectPartKind::Flesh => (),
            InsectPartKind::Head | InsectPartKind::Legs => return false,
        };

        body.adjacent_parts(part.position)
            .any(|opt_part| match opt_part {
                None => true,
                Some(part) => matches!(part.kind, InsectPartKind::Head | InsectPartKind::Legs),
            })
    };

    // not a closure because `|a: &u32| -> &u32 { a }` doesnt compile and there is no way to annotate the fn sig
    // without either `for<'a> |a: &'a u32| -> &'a u32` syntax, or `let a: impl Fn(&u32) -> &u32` syntax.
    fn pick_edge_flesh<'a>(
        filter: impl Fn(&InsectBody, &&InsectPart) -> bool,
        body: &'a InsectBody,
    ) -> &'a InsectPart {
        let hard_insect_parts = body.parts.iter().filter(|part| filter(body, part)).count();
        assert!(hard_insect_parts > 0);
        body.parts
            .iter()
            .filter(|part| filter(body, part))
            .nth(rand::thread_rng().gen_range(0..hard_insect_parts))
            .unwrap()
    }

    let a_flesh = pick_edge_flesh(filter, a);
    let b_flesh = pick_edge_flesh(filter, b);

    let mut pad_x_start = 0;
    let mut pad_y_start = 0;
    if b_flesh.position.0 > a_flesh.position.0 {
        pad_x_start = b_flesh.position.0 - a_flesh.position.0;
    }
    if b_flesh.position.1 > a_flesh.position.1 {
        pad_y_start = b_flesh.position.1 - a_flesh.position.1;
    }

    // FIXME we need to allow for rotating `b` before placement
    let mut wip_insect_parts = Vec::<InsectPart>::new();
    for part in a.parts.iter() {
        wip_insect_parts.push(InsectPart {
            position: (part.position.0 + pad_x_start, part.position.1 + pad_y_start),
            ..*part
        })
    }
    use InsectPartKind::*;
    for part in b.parts.iter() {
        let offset_x = (a_flesh.position.0 + pad_x_start) - b_flesh.position.0;
        let offset_y = (a_flesh.position.1 + pad_y_start) - b_flesh.position.1;
        let new_pos = (part.position.0 + offset_x, part.position.1 + offset_y);
        match wip_insect_parts
            .iter_mut()
            .find(|part| part.position == new_pos)
        {
            None => (),
            Some(existing_part) => match (existing_part.kind, part.kind) {
                (Flesh, Head | Legs) => continue,
                (Flesh, Flesh) | (Head, _) | (Legs, _) => {
                    *existing_part = InsectPart {
                        position: new_pos,
                        ..*part
                    };
                    continue;
                }
            },
        };

        wip_insect_parts.push(InsectPart {
            position: new_pos,
            ..*part
        });
    }

    InsectBody::new(wip_insect_parts)
}

pub fn generate_body(sources: &[InsectBody], generations: u8) -> InsectBody {
    let (lhs, rhs) = match generations {
        0 => (
            sources[rand::thread_rng().gen_range(0..sources.len())].clone(),
            sources[rand::thread_rng().gen_range(0..sources.len())].clone(),
        ),
        _ => (
            generate_body(sources, generations - 1),
            generate_body(sources, generations - 1),
        ),
    };
    merge_insect_bodies(&lhs, &rhs)
}
