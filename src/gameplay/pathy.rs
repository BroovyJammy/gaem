use bevy::utils::{hashbrown::hash_map::Entry, StableHashMap, StableHashSet};

use crate::{asset::TerrainDescriptor, prelude::*};

use super::{insect_body::InsectBody, map::TerrainKind, MoveCap, TerrainInfo, UnitPos};

pub fn get_movable_to_tiles<'a, I>(
    entity: Entity,
    starting_pos: IVec2,
    body: &InsectBody,
    move_cap: MoveCap,
    units: impl Fn() -> I + Copy + 'a,
    terrain_info: &TerrainInfo<'_, '_>,
) -> StableHashSet<UVec2>
where
    I: Iterator<Item = (Entity, &'a UnitPos, &'a InsectBody)>,
{
    let terrain_map = terrain_info.get_terrain_map();
    get_movable_through_tiles(entity, starting_pos, body, move_cap, units, terrain_info)
        .into_iter()
        .filter(|(pos, _)| {
            is_place_valid_to_be(
                true,
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
    move_cap: MoveCap,
    units: impl Fn() -> I + 'a,
    terrain_info: &TerrainInfo<'_, '_>,
) -> StableHashMap<UVec2, u64>
where
    I: Iterator<Item = (Entity, &'a UnitPos, &'a InsectBody)>,
{
    assert!(starting_pos.x >= 0 && starting_pos.y >= 0);
    let starting_pos = starting_pos.as_uvec2();
    let terrain_map = terrain_info.get_terrain_map();
    let terrain_descriptor = terrain_info.get_terrain_descriptor(terrain_map);

    let mut scores = StableHashMap::<UVec2, u64>::with_hasher(default());
    let mut todo_list = vec![(starting_pos, 0_u64)];

    while let Some((pos, score)) = todo_list.pop() {
        if score > move_cap.0 as u64 {
            continue;
        }

        let entry = scores.entry(pos.into());
        if let Entry::Occupied(existing_score) = &entry {
            if *existing_score.get() < score {
                continue;
            }
        } else if !is_place_valid_to_be(
            false,
            entity,
            body,
            pos.as_ivec2(),
            units(),
            terrain_descriptor,
        ) {
            continue;
        }

        entry.insert(score);

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
            todo_list.push((offsetted.as_uvec2(), score + 1));
        }
    }

    scores
}

pub fn is_place_valid_to_be<'a, 'b>(
    check_unit_overlap: bool,
    mover: Entity,
    mover_body: &InsectBody,
    place: IVec2,
    units: impl Iterator<Item = (Entity, &'a UnitPos, &'a InsectBody)>,
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
    let overlaps_units = match check_unit_overlap {
        true => units
            .into_iter()
            .filter(|&(e, _, _)| e != mover)
            .any(|(_, pos, body)| body.intersects(*pos, mover_body, UnitPos(place))),
        false => false,
    };
    if overlaps_units {
        return false;
    }
    true
}
