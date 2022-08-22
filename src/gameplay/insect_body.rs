use bevy::{
    prelude::*,
    sprite::Anchor,
    utils::{HashMap, HashSet},
};
use rand::{rngs::StdRng, Rng};

use crate::{asset::MapAssets, map::TILE_SIZE};

use super::{Team, UnitPos};

#[derive(Clone, Copy, Debug)]
pub enum PartDirection {
    Right = 3,
    Down = 2,
    _Left = 1,
    Up = 0,
}
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InsectPartKind {
    Flesh = 0,
    Head = 1,
    Legs = 2,
}

impl InsectPartKind {
    fn move_bonus(self) -> u32 {
        match self {
            Self::Legs => 3,
            _ => 0,
        }
    }

    fn max_health(self) -> u32 {
        match self {
            Self::Flesh => 3,
            Self::Head => 2,
            Self::Legs => 2,
        }
    }

    pub fn damage(self) -> u32 {
        match self {
            Self::Head => 1,
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct InsectPart {
    pub position: (u32, u32),
    pub kind: InsectPartKind,
    pub rotation: PartDirection,
    pub health: u32,
}
impl InsectPart {
    pub fn new(pos: (u32, u32), kind: InsectPartKind, rot: PartDirection) -> Self {
        Self {
            position: pos,
            kind,
            rotation: rot,
            health: kind.max_health(),
        }
    }
}

#[derive(Component, Clone)]
pub struct InsectBody {
    pub parts: Vec<InsectPart>,
    pub used_tiles: HashSet<(u32, u32)>,
}

impl InsectBody {
    pub fn new(parts: Vec<InsectPart>) -> Self {
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
        self.parts
            .iter()
            .map(|part| part.kind.move_bonus())
            .sum::<u32>()
    }

    pub fn get_part(&self, local_tile: (u32, u32)) -> Option<&InsectPart> {
        self.parts.iter().find(|part| part.position == local_tile)
    }

    pub fn get_part_mut(&mut self, local_tile: (u32, u32)) -> Option<&mut InsectPart> {
        self.parts
            .iter_mut()
            .find(|part| part.position == local_tile)
    }

    pub fn remove_part(&mut self, local_tile: (u32, u32)) {
        self.parts.retain(|part| part.position != local_tile);
        self.used_tiles.remove(&local_tile);
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

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct UpdateBody;

#[derive(Component)]
/// Used to keep track of the render entities corresponding to each insect part
pub struct InsectRenderEntities {
    pub hp_bar: HashMap<(u32, u32), Entity>,
    pub body_part: HashMap<(u32, u32), Entity>,
}

pub fn update_insect_body_tilemap(
    mut cmds: Commands<'_, '_>,
    mut insects: Query<(Entity, &InsectBody, &Team, &mut InsectRenderEntities), With<UpdateBody>>,
    assets: Res<MapAssets>,
) {
    for (entity, body, team, mut render_body_parts) in insects.iter_mut() {
        for child in render_body_parts.body_part.values() {
            cmds.entity(*child).despawn_recursive();
        }
        for child in render_body_parts.hp_bar.values() {
            cmds.entity(*child).despawn_recursive();
        }
        render_body_parts.body_part.clear();
        render_body_parts.hp_bar.clear();

        cmds.entity(entity)
            .with_children(|child_builder| {
                for part in body.parts.iter() {
                    let body_part = child_builder
                        .spawn_bundle(SpriteSheetBundle {
                            sprite: TextureAtlasSprite {
                                index: part.kind as usize,
                                color: team.color(),
                                anchor: Anchor::Center,
                                ..default()
                            },
                            transform: Transform {
                                translation: {
                                    Vec3::new(
                                        (part.position.0 * TILE_SIZE + TILE_SIZE / 2) as f32,
                                        (part.position.1 * TILE_SIZE + TILE_SIZE / 2) as f32,
                                        0.0,
                                    )
                                },
                                rotation: Quat::from_axis_angle(
                                    Vec3::Z,
                                    part.rotation as u8 as f32 * std::f32::consts::FRAC_PI_2,
                                ),
                                ..default()
                            },
                            texture_atlas: assets.insect.clone(),
                            ..default()
                        })
                        .id();
                    render_body_parts.body_part.insert(part.position, body_part);

                    let hp = child_builder
                        .spawn_bundle(SpriteBundle {
                            sprite: Sprite {
                                color: Color::rgba(0.4, 0.0, 0.0, 0.5),
                                flip_x: false,
                                flip_y: false,
                                custom_size: Some(Vec2::new(
                                    TILE_SIZE as f32,
                                    TILE_SIZE as f32
                                        - (TILE_SIZE as f32
                                            * (part.health as f32 / part.kind.max_health() as f32)),
                                )),
                                anchor: Anchor::BottomLeft,
                            },
                            transform: Transform {
                                translation: {
                                    Vec3::new(
                                        (part.position.0 * TILE_SIZE) as f32,
                                        (part.position.1 * TILE_SIZE) as f32,
                                        1.0,
                                    )
                                },
                                ..default()
                            },
                            ..default()
                        })
                        .id();
                    render_body_parts.hp_bar.insert(part.position, hp);
                }
            })
            .remove::<UpdateBody>();
    }
}

pub fn merge_insect_bodies(a: &InsectBody, b: &InsectBody, rng: &mut StdRng) -> InsectBody {
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
        rng: &mut StdRng,
    ) -> &'a InsectPart {
        let hard_insect_parts = body.parts.iter().filter(|part| filter(body, part)).count();
        assert!(hard_insect_parts > 0);
        body.parts
            .iter()
            .filter(|part| filter(body, part))
            .nth(rng.gen_range(0..hard_insect_parts))
            .unwrap()
    }

    let a_flesh = pick_edge_flesh(filter, a, rng);
    let b_flesh = pick_edge_flesh(filter, b, rng);

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

pub fn generate_body(sources: &[InsectBody], generations: u8, rng: &mut StdRng) -> InsectBody {
    let (lhs, rhs) = match generations {
        0 => (
            sources[rng.gen_range(0..sources.len())].clone(),
            sources[rng.gen_range(0..sources.len())].clone(),
        ),
        _ => (
            generate_body(sources, generations - 1, rng),
            generate_body(sources, generations - 1, rng),
        ),
    };
    merge_insect_bodies(&lhs, &rhs, rng)
}
