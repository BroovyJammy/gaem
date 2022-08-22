use super::{map::TILE_SIZE, MoveCap, Team, UnitPos};
use crate::asset::{BodyParts, MapAssets};
use crate::prelude::*;
use bevy::{
    sprite::Anchor,
    utils::{HashMap, HashSet},
};
use rand::{rngs::StdRng, Rng};

#[derive(Clone, Copy, Debug)]
pub enum PartDirection {
    Right = 3,
    Down = 2,
    Left = 1,
    Up = 0,
}

impl PartDirection {
    pub fn to_u8(self) -> u8 {
        self as u8
    }

    pub fn from_u8(u8: u8) -> Self {
        match u8 {
            0 => Self::Up,
            1 => Self::Left,
            2 => Self::Down,
            3 => Self::Right,
            _ => unreachable!(),
        }
    }

    pub fn rotate_ivec(&self, mut ivec: IVec2) -> IVec2 {
        for _ in 0..self.to_u8() {
            ivec = IVec2::new(-ivec.y, ivec.x)
        }
        ivec
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct InsectPartKind(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct InsectPart {
    pub position: (u32, u32),
    pub kind: InsectPartKind,
    pub rotation: PartDirection,
    pub health: u32,
}

#[derive(Component, Clone)]
pub struct InsectBody {
    pub parts: Vec<InsectPart>,
    pub used_tiles: HashSet<(u32, u32)>,
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

impl InsectPart {
    pub fn new(
        pos: (u32, u32),
        kind: InsectPartKind,
        rot: PartDirection,
        stats: &BodyParts,
    ) -> Self {
        Self {
            position: pos,
            kind,
            rotation: rot,
            health: stats.0[kind.0].max_health,
        }
    }
}

impl InsectBody {
    pub fn new(parts: Vec<InsectPart>) -> Self {
        let used_tiles = parts.iter().map(|part| part.position).collect();
        Self { parts, used_tiles }
    }

    /// `tile` is in world space
    pub fn contains_tile(&self, insect_position: UnitPos, tile: IVec2) -> bool {
        let tile = tile - insect_position.0;
        if tile.x < 0 || tile.y < 0 {
            return false;
        }

        self.used_tiles.contains(&(tile.x as u32, tile.y as u32))
    }

    pub fn max_move_cap(&self, stats: &BodyParts) -> u32 {
        self.parts
            .iter()
            .map(|part| stats[part.kind].move_bonus)
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
    pub fn _adjacent_parts(
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

    pub fn make_new_rotated(&self, dir: PartDirection) -> Self {
        let mut largest = 0;
        for (x, y) in self.parts.iter().map(|part| part.position) {
            if x > largest {
                largest = x;
            }
            if y > largest {
                largest = y;
            }
        }

        let mut body_parts = vec![];
        for part in self.parts.iter() {
            let mut pos = part.position;
            let mut rot = part.rotation;
            for _ in 0..=dir.to_u8() {
                pos = (pos.1, largest - pos.0);
                rot = match rot {
                    PartDirection::Right => PartDirection::Down,
                    PartDirection::Down => PartDirection::Left,
                    PartDirection::Left => PartDirection::Up,
                    PartDirection::Up => PartDirection::Right,
                }
            }
            body_parts.push(InsectPart {
                position: pos,
                kind: part.kind,
                rotation: rot,
                health: part.health,
            })
        }

        Self::new(body_parts)
    }
}

pub fn spawn_insect(
    commands: &mut Commands,
    pos: IVec2,
    body: InsectBody,
    team: Team,
    move_cap: MoveCap,
) {
    commands
        .spawn()
        .insert_bundle(TransformBundle { ..default() })
        .insert(UnitPos(pos))
        .insert(body)
        .insert(UpdateBody)
        .insert(team)
        .insert(InsectRenderEntities {
            hp_bar: HashMap::new(),
            body_part: HashMap::new(),
        })
        .insert(move_cap)
        .insert_bundle(VisibilityBundle { ..default() });
}

pub fn update_insect_body_tilemap(
    mut cmds: Commands<'_, '_>,
    mut insects: Query<(Entity, &InsectBody, &Team, &mut InsectRenderEntities), With<UpdateBody>>,
    assets: Res<MapAssets>,
    stats: Res<BodyParts>,
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
                                index: stats[part.kind].sprite_idx,
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
                                            * (part.health as f32
                                                / stats[part.kind].max_health as f32)),
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

// They did the monster mash...
pub fn merge_insect_bodies(
    a: &InsectBody,
    b: &InsectBody,
    rng: &mut StdRng,
    stats: &BodyParts,
) -> InsectBody {
    let dir = PartDirection::from_u8(rng.gen_range(0..4));
    let b = b.make_new_rotated(dir);

    let filter = |body: &InsectBody, part: &&InsectPart, stats: &BodyParts| {
        stats[part.kind]
            .connections
            .iter()
            .map(|&c| UVec2::from(part.position).as_ivec2() + part.rotation.rotate_ivec(c))
            .any(|adjacent| {
                if adjacent.x < 0 || adjacent.y < 0 {
                    return false;
                }
                !body
                    .used_tiles
                    .contains(&(adjacent.x as u32, adjacent.y as u32))
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

    let a_flesh = pick_edge_flesh(|a, b| filter(a, b, stats), a, rng);
    let b_flesh = pick_edge_flesh(|a, b| filter(a, b, stats), &b, rng);

    let mut pad_x_start = 0;
    let mut pad_y_start = 0;
    if b_flesh.position.0 > a_flesh.position.0 {
        pad_x_start = b_flesh.position.0 - a_flesh.position.0;
    }
    if b_flesh.position.1 > a_flesh.position.1 {
        pad_y_start = b_flesh.position.1 - a_flesh.position.1;
    }

    let mut wip_insect_parts = Vec::<InsectPart>::new();
    for part in a.parts.iter() {
        wip_insect_parts.push(InsectPart {
            position: (part.position.0 + pad_x_start, part.position.1 + pad_y_start),
            ..*part
        })
    }

    for part in b.parts.iter() {
        let offset_x = (a_flesh.position.0 + pad_x_start) - b_flesh.position.0;
        let offset_y = (a_flesh.position.1 + pad_y_start) - b_flesh.position.1;
        let new_pos = (part.position.0 + offset_x, part.position.1 + offset_y);
        if let Some(existing_part) = wip_insect_parts
            .iter_mut()
            .find(|part| part.position == new_pos)
        {
            // we want to ensure that both units are still "well formed" i.e. do not have any detached
            // parts after the merging process. to ensure this, whenever we would place a part overlapping
            // another part, we first check if the existing part has a superset of the connections that the
            // part we are placing has. If it does we don't overwrite it, if both of the two parts have
            // a connection that the other does not, then we ''upgrade'' the part to a piece of flesh which has
            // all 4 connections.

            let existing_part_connections = stats[existing_part.kind]
                .connections
                .iter()
                .map(|&c| existing_part.rotation.rotate_ivec(c))
                .collect::<HashSet<IVec2>>();
            let incoming_part_connections = stats[part.kind]
                .connections
                .iter()
                .map(|&c| part.rotation.rotate_ivec(c))
                .collect::<HashSet<IVec2>>();

            if existing_part_connections.is_subset(&incoming_part_connections) {
                *existing_part = InsectPart {
                    position: new_pos,
                    ..*part
                };
            } else if incoming_part_connections.is_subset(&existing_part_connections) {
                // do nothing
            } else {
                *existing_part = InsectPart {
                    position: new_pos,
                    kind: InsectPartKind(0), // spider flesh
                    rotation: part.rotation,
                    health: stats[InsectPartKind(0)].max_health,
                }
            }
            continue;
        };

        wip_insect_parts.push(InsectPart {
            position: new_pos,
            ..*part
        });
    }

    InsectBody::new(wip_insect_parts)

    // ...It was a graveyard smash
}

pub fn generate_body(
    sources: &[InsectBody],
    generations: u8,
    rng: &mut StdRng,
    stats: &BodyParts,
) -> InsectBody {
    let (lhs, rhs) = match generations {
        0 => return sources[rng.gen_range(0..sources.len())].clone(),
        _ => (
            generate_body(sources, generations - 1, rng, stats),
            generate_body(sources, generations - 1, rng, stats),
        ),
    };
    merge_insect_bodies(&lhs, &rhs, rng, stats)
}

#[cfg(test)]
#[test]
fn rotation_works() {
    let my_vec = IVec2::new(0, 1);
    assert_eq!(PartDirection::Up.rotate_ivec(my_vec), IVec2::new(0, 1));
    assert_eq!(PartDirection::Left.rotate_ivec(my_vec), IVec2::new(-1, 0));
    assert_eq!(PartDirection::Down.rotate_ivec(my_vec), IVec2::new(0, -1));
    assert_eq!(PartDirection::Right.rotate_ivec(my_vec), IVec2::new(1, 0));
}
