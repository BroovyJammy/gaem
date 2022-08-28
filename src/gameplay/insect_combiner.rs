//! Not to be confused with `insect_body.rs` which contains logic for merging insects

use bevy::utils::StableHashMap;
use rand::{rngs::StdRng, Rng, SeedableRng};

use crate::{
    asset::BodyParts,
    cutscene::CurrentCutscene,
    prelude::*,
    ui::{CombineButton, CombineNext, CombinePrev, CombineText, CombineWrapper},
};

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
    level_gameplay_info: Res<LevelGameplayInfo>,
    mut unit_to_combine: ResMut<UnitToCombine>,
    stats: Res<BodyParts>,
    mut camera: Query<&mut Transform, With<Camera>>,
) {
    let mut camera_trans = camera.single_mut();
    camera_trans.translation.x = 0.;
    camera_trans.translation.y = 0.;

    unit_to_combine.0 = 0;
    let goodie = super::spawn_insect(
        &mut commands,
        IVec2::new(0, 0),
        current_units.0[0].clone(),
        Team::Goodie,
        MoveCap(current_units.0[0].clone().max_move_cap(&stats)),
        default(),
    );
    commands.entity(goodie).insert_bundle(TransformBundle {
        // Why does this value have to be so large?
        local: Transform::from_xyz(-200., 0., 0.),
        ..default()
    });

    let body =
        &level_gameplay_info.find_root_unsplit_insect_for(level_gameplay_info.last_killed.unwrap());
    let baddie = super::spawn_insect(
        &mut commands,
        IVec2::new(0, 0),
        body.clone(),
        Team::Baddie,
        MoveCap(body.max_move_cap(&stats)),
        default(),
    );
    commands.entity(baddie).insert_bundle(TransformBundle {
        local: Transform::from_xyz(200., 0., 0.),
        ..default()
    });
}

pub fn change_selected_unit(
    actioners: Query<&ActionState<Action>>,
    prevs: Query<&Interaction, (With<CombinePrev>, Changed<Interaction>)>,
    nexts: Query<&Interaction, (With<CombineNext>, Changed<Interaction>)>,
    mut unit_to_combine: ResMut<UnitToCombine>,
    current_units: Res<CurrentUnits>,
) {
    const MAX_UNITS: u8 = 4;

    let input = actioners.single();
    let prev = prevs
        .get_single()
        .map(|prev| *prev == Interaction::Clicked)
        .unwrap_or(false);
    let next = nexts
        .get_single()
        .map(|next| *next == Interaction::Clicked)
        .unwrap_or(false);
    if input.just_pressed(Action::MoveSelection) || prev || next {
        let movement = input
            .clamped_axis_pair(Action::MoveSelection)
            .unwrap() // no idea when this is None :shrug:
            .x()
            - prev as i32 as f32
            + next as i32 as f32;
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
    mut combine_texts: Query<&mut Text, With<CombineText>>,
    mut body: Query<(Entity, &mut InsectBody, &mut InsectRenderEntities, &Team)>,
) {
    if !unit_to_combine.is_changed() {
        return;
    }

    for (entity, mut cur_body, mut render_entities, team) in &mut body {
        if let Team::Baddie = *team {
            continue;
        }

        let text = &mut combine_texts.single_mut().sections[0].value;

        match current_units.0.get(unit_to_combine.0) {
            None => {
                *text = "ADD TO PARTY".to_string();
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
                // This could be done way better :P
                *text = "      COMBINE      ".to_string();
                *cur_body = selected_body.clone();
                commands.entity(entity).insert(UpdateBody);
            }
        }
    }
}

#[derive(Default, Deref, DerefMut)]
pub struct Buffer(bool);

pub fn confirm_combine_unit(
    mut commands: Commands<'_, '_>,
    mut buffer: Local<Buffer>,
    actioner: Query<&ActionState<Action>>,
    combine_buttons: Query<&Interaction, (With<CombineButton>, Changed<Interaction>)>,
    unit_to_combine: Res<UnitToCombine>,
    mut current_units: ResMut<CurrentUnits>,
    level_gameplay_info: Res<LevelGameplayInfo>,
    stats: Res<BodyParts>,
    levels: Res<Levels>,
    mut current_level: ResMut<CurrentLevel>,
) {
    // Skip a frame to ignore the input that caused the transition to this scene
    if !**buffer {
        **buffer = true;
        return;
    }

    if actioner.single().just_pressed(Action::Confirm)
        || combine_buttons
            .iter()
            .any(|button| *button == Interaction::Clicked)
    {
        info!(
            "Used keyboard/controller: {}",
            actioner.single().just_pressed(Action::Confirm)
        );

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

        let level = &levels[current_level.0];
        if let Some(cutscene) = &level.post_cutscene {
            **buffer = false;
            commands.insert_resource(CurrentCutscene::new(cutscene));
            commands.insert_resource(NextState(AppState::PlayCutscene));
            current_level.0 += 1;
        } else {
            debug!("finished level but there was no cutscene to move to");
        }
    }
}

pub fn cleanup_insect_combiner(
    mut commands: Commands<'_, '_>,
    items: Query<Entity, Or<(With<InsectBody>, With<CombineWrapper>)>>,
) {
    for item in &items {
        commands.entity(item).despawn_recursive();
    }
}
