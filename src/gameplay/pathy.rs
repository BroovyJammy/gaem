use std::collections::VecDeque;

use bevy::utils::{hashbrown::hash_map::Entry, StableHashMap, StableHashSet};

use crate::{asset::TerrainDescriptor, prelude::*};

use super::{
    insect_body::InsectBody, map::TerrainKind, MoveCap, MoveTo, Team, TerrainInfo, UnitPos,
};

#[derive(Copy, Clone)]
pub enum CollideWith {
    Team(Team),
    All,
}

pub fn get_path(map: StableHashMap<UVec2, u64>, from: UVec2, to: UVec2) -> Vec<UVec2> {
    assert!(map[&from] == 0);
    let mut path = vec![to];

    while *path.last().unwrap() != from {
        let mut lowest = u64::MAX;
        let mut tile = None;
        for adjacent in [
            IVec2::new(-1, 0),
            IVec2::new(1, 0),
            IVec2::new(0, -1),
            IVec2::new(0, 1),
        ] {
            let offseted = path.last().unwrap().as_ivec2() + adjacent;
            if offseted.x < 0 || offseted.y < 0 {
                continue;
            }
            if let Some(score) = map.get(&offseted.as_uvec2()).copied() {
                if score < lowest {
                    lowest = score;
                    tile = Some(offseted.as_uvec2());
                }
            }
        }
        path.push(tile.unwrap());
    }

    path
}

pub fn ghost_updated_units<'a>(
    units: impl Iterator<
            Item = (
                Entity,
                &'a UnitPos,
                &'a InsectBody,
                &'a Team,
                Option<&'a MoveTo>,
            ),
        > + 'a,
    ghosts: impl Fn(Entity) -> &'a UnitPos + 'a,
) -> impl Iterator<Item = (Entity, &'a UnitPos, &'a InsectBody, &'a Team)> + 'a {
    units.map(move |(a, b, c, d, move_to)| match move_to {
        Some(&MoveTo(ghost)) => {
            let b = ghosts(ghost);
            (a, b, c, d)
        }
        None => (a, b, c, d),
    })
}

pub fn get_movable_to_tiles<'a, I>(
    entity: Entity,
    starting_pos: IVec2,
    body: &InsectBody,
    collide_with: CollideWith,
    move_cap: MoveCap,
    units: impl Fn() -> I + Copy + 'a,
    terrain_info: &TerrainInfo<'_, '_>,
) -> StableHashSet<UVec2>
where
    I: Iterator<Item = (Entity, &'a UnitPos, &'a InsectBody, &'a Team)>,
{
    let terrain_map = terrain_info.get_terrain_map();
    get_movable_through_tiles(
        entity,
        starting_pos,
        body,
        collide_with,
        move_cap,
        units,
        terrain_info,
    )
    .into_iter()
    .filter(|(pos, _)| {
        is_place_valid_to_be(
            CollideWith::All,
            entity,
            body,
            pos.as_ivec2(),
            units(),
            terrain_info.get_terrain_descriptor(terrain_map),
        )
    })
    .map(|(pos, _)| pos)
    .collect()
}

pub fn get_movable_through_tiles<'a, I>(
    entity: Entity,
    starting_pos: IVec2,
    body: &InsectBody,
    collide_with: CollideWith,
    move_cap: MoveCap,
    units: impl Fn() -> I + 'a,
    terrain_info: &TerrainInfo<'_, '_>,
) -> StableHashMap<UVec2, u64>
where
    I: Iterator<Item = (Entity, &'a UnitPos, &'a InsectBody, &'a Team)>,
{
    assert!(starting_pos.x >= 0 && starting_pos.y >= 0);
    let starting_pos = starting_pos.as_uvec2();
    let terrain_map = terrain_info.get_terrain_map();
    let terrain_descriptor = terrain_info.get_terrain_descriptor(terrain_map);

    let mut scores = StableHashMap::with_hasher(default());
    scores.insert(starting_pos, 0u64);
    let mut todo_list = VecDeque::new();
    todo_list.push_front((starting_pos, 0u64));

    while let Some((pos, score)) = todo_list.pop_back() {
        for offset in [
            IVec2::new(-1, 0),
            IVec2::new(1, 0),
            IVec2::new(0, -1),
            IVec2::new(0, 1),
        ] {
            let offsetted = pos.as_ivec2() + offset;
            if offsetted.x < 0
                || offsetted.y < 0
                || offsetted.x >= terrain_info.level_info.level().size_x as i32
                || offsetted.y >= terrain_info.level_info.level().size_y as i32
            {
                continue;
            }

            if score + 1 > move_cap.0 as u64 {
                continue;
            }

            let entry = scores.entry(offsetted.as_uvec2());
            if let Entry::Occupied(existing_score) = &entry {
                if *existing_score.get() <= score + 1 {
                    continue;
                }
            }

            // only check for collisions when the entry isnt present, as if it _is_ present
            // then its valid for the insect to be positioned there so this would be redundant.
            if matches!(&entry, Entry::Vacant(_))
                && !is_place_valid_to_be(
                    collide_with,
                    entity,
                    body,
                    pos.as_ivec2(),
                    units(),
                    terrain_descriptor,
                )
            {
                continue;
            }
            entry.insert(score + 1);

            todo_list.push_front((offsetted.as_uvec2(), score + 1));
        }
    }

    scores
}

pub fn is_place_valid_to_be<'a, 'b>(
    collide_with: CollideWith,
    mover: Entity,
    mover_body: &InsectBody,
    place: IVec2,
    units: impl Iterator<Item = (Entity, &'a UnitPos, &'a InsectBody, &'a Team)>,
    terrain_info: impl Fn(UnitPos) -> Option<(TerrainKind, &'b TerrainDescriptor)> + 'b,
) -> bool {
    let overlaps_terrain = mover_body
        .used_tiles
        .iter()
        .map(|&(local_tile_pos_x, local_tile_pos_y)| {
            UnitPos(IVec2::new(
                local_tile_pos_x as i32 + place.x,
                local_tile_pos_y as i32 + place.y,
            ))
        })
        .any(|used_tile| {
            let info = terrain_info(used_tile);
            info.is_none() || info.unwrap().1.wall
        });
    if overlaps_terrain {
        return false;
    }

    let overlaps_units = match collide_with {
        CollideWith::Team(team) => units
            .into_iter()
            .filter(|&(e, _, _, other_team)| e != mover && team == *other_team)
            .any(|(_, pos, body, _)| body.intersects(*pos, mover_body, UnitPos(place))),
        CollideWith::All => units
            .into_iter()
            .filter(|&(e, _, _, _)| e != mover)
            .any(|(_, pos, body, _)| body.intersects(*pos, mover_body, UnitPos(place))),
    };
    if overlaps_units {
        return false;
    }
    true
}
