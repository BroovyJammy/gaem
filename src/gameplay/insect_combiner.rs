//! Not to be confused with `insect_body.rs` which contains logic for merging insects

use bevy::utils::StableHashMap;
use rand::{rngs::StdRng, Rng, SeedableRng};

use crate::{asset::BodyParts, cutscene::CurrentCutscene, prelude::*};

use super::{
    insect_body::{InsectBody, InsectRenderEntities, UpdateBody},
    Action, CurrentLevel, CurrentUnits, Levels, MoveCap, Team,
};

/// Information about a playthrough of a level
pub struct LevelGameplayInfo {
    generated_enemies: Vec<(Entity, InsectBody)>,
    splitted_to_unsplit: StableHashMap<Entity, Entity>,
    last_killed: Option<Entity>,
}

impl LevelGameplayInfo {
    pub fn new() -> Self {
        Self {
            generated_enemies: Vec::new(),
            splitted_to_unsplit: StableHashMap::with_hasher(default()),
            last_killed: None,
        }
    }

    pub fn clear(&mut self) {
        self.generated_enemies.clear();
        self.splitted_to_unsplit.clear();
        self.last_killed = None;
    }

    pub fn set_generated_enemies(&mut self, generated_enemies: Vec<(Entity, InsectBody)>) {
        self.generated_enemies = generated_enemies;
    }

    pub fn insert_insect_split(
        &mut self,
        insect: Entity,
        split_into: impl Iterator<Item = Entity>,
    ) {
        self.splitted_to_unsplit
            .extend(split_into.map(|splitted| (splitted, insect)));
    }

    pub fn find_root_unsplit_insect_for(&self, mut splitten: Entity) -> InsectBody {
        for (entity, _) in &self.generated_enemies {
            debug!("generated_enemy: {:?}", entity);
        }
        debug!("splitted_to_unsplit: {:?}", &self.splitted_to_unsplit);
        while let Some(unsplit) = self.splitted_to_unsplit.get(&splitten) {
            debug!("step: splitten equals {:?}", splitten);
            splitten = *unsplit;
            debug!("step: *unsplit equals {:?}", *unsplit);
        }
        self.generated_enemies
            .iter()
            .find(|&&(entity, _)| entity == splitten)
            .unwrap()
            .1
            .clone()
    }

    pub fn set_last_killed(&mut self, killed: Entity) {
        self.last_killed = Some(killed);
    }
}

pub struct UnitToCombine(pub usize);

pub fn setup_insect_combiner(
    mut commands: Commands<'_, '_>,
    current_units: Res<CurrentUnits>,
    mut unit_to_combine: ResMut<UnitToCombine>,
    stats: Res<BodyParts>,
    mut camera: Query<&mut Transform, With<Camera>>,
) {
    let mut camera_trans = camera.single_mut();
    camera_trans.translation.x = 0.;
    camera_trans.translation.y = 0.;

    unit_to_combine.0 = 0;
    super::spawn_insect(
        &mut commands,
        IVec2::new(0, 0),
        current_units.0[0].clone(),
        Team::Goodie,
        MoveCap(current_units.0[0].clone().max_move_cap(&stats)),
        default(),
    );
}

pub fn change_selected_unit(
    actioners: Query<&ActionState<Action>>,
    mut unit_to_combine: ResMut<UnitToCombine>,
    current_units: Res<CurrentUnits>,
) {
    const MAX_UNITS: u8 = 4;

    let input = actioners.single();
    if input.just_pressed(Action::MoveSelection) {
        let movement = input
            .clamped_axis_pair(Action::MoveSelection)
            .unwrap() // no idea when this is None :shrug:
            .x();
        let new_unit = if movement < 0. {
            let new_unit = unit_to_combine.0.checked_sub(1);
            match new_unit {
                None => {
                    let new_unit = current_units.0.len();
                    if new_unit == MAX_UNITS as usize {
                        new_unit - 1
                    } else {
                        new_unit
                    }
                }
                Some(new_unit) => new_unit,
            }
        } else {
            let new_unit = unit_to_combine.0 + 1;
            if new_unit == MAX_UNITS as usize || new_unit > current_units.0.len() {
                0
            } else {
                new_unit
            }
        };

        unit_to_combine.0 = new_unit;
    }
}

pub fn update_to_combine_unit_visuals(
    mut commands: Commands<'_, '_>,
    unit_to_combine: Res<UnitToCombine>,
    current_units: Res<CurrentUnits>,
    mut body: Query<(Entity, &mut InsectBody, &mut InsectRenderEntities)>,
) {
    if !unit_to_combine.is_changed() {
        return;
    }

    let (entity, mut cur_body, mut render_entities) = body.single_mut();
    match current_units.0.get(unit_to_combine.0) {
        None => {
            let render_entities = &mut *render_entities;
            for &entity in render_entities
                .body_part
                .values()
                .chain(render_entities.hp_bar.values())
            {
                commands.entity(entity).despawn_recursive();
            }
            render_entities.body_part.clear();
            render_entities.hp_bar.clear();
        }
        Some(selected_body) => {
            *cur_body = selected_body.clone();
            commands.entity(entity).insert(UpdateBody);
        }
    }
}

pub fn confirm_combine_unit(
    mut commands: Commands<'_, '_>,
    actioner: Query<&ActionState<Action>>,
    unit_to_combine: Res<UnitToCombine>,
    mut current_units: ResMut<CurrentUnits>,
    level_gameplay_info: Res<LevelGameplayInfo>,
    stats: Res<BodyParts>,
) {
    if actioner.single().just_pressed(Action::Select) {
        let enemy_body = level_gameplay_info
            .find_root_unsplit_insect_for(level_gameplay_info.last_killed.unwrap());
        let blah = current_units.0.get_mut(unit_to_combine.0);
        debug!("blah.is_some(): {}", blah.is_some());
        match blah {
            None => current_units.0.push(enemy_body),
            Some(selected_body) => {
                let seed = rand::thread_rng().gen();
                debug!("seed: {}", seed);
                let (Ok(new_body) | Err(new_body)) = super::insect_body::merge_insect_bodies(
                    selected_body,
                    &enemy_body,
                    &mut StdRng::seed_from_u64(seed),
                    &stats,
                );
                *selected_body = new_body;
            }
        }

        commands.insert_resource(NextState(AppState::PlayCutscene));
    }
}

pub fn cleanup_insect_combiner(
    mut commands: Commands<'_, '_>,
    units: Query<Entity, With<InsectBody>>,
) {
    commands.entity(units.single()).despawn_recursive();
}
