use bevy::prelude::*;
use iyes_loopless::prelude::*;

use crate::{map::TileSelected, state::GameState};

pub struct GameplayPlugin;

impl Plugin for GameplayPlugin {
    fn build(&self, app: &mut App) {
        app.add_system(select_unit.run_in_state(GameState::Game))
            .add_system(
                move_unit
                    .run_in_state(GameState::Game)
                    .run_if_resource_exists::<SelectedUnit>(),
            )
            // Awesome temporary system
            .add_system(|mut e: EventReader<MoveUnit>| {
                for e in e.iter() {
                    info!("{e:?}");
                }
            });
    }
}

// Resource that only exists when a unit is selected
#[derive(Clone, Copy, Deref)]
struct SelectedUnit(Entity);

#[derive(Component, Deref)]
struct UnitPos(UVec2);

// Event to the tilemap
#[derive(Debug)]
pub struct MoveUnit {
    from: UVec2,
    to: UVec2,
}

fn select_unit(
    mut commands: Commands,
    mut selected_tiles: EventReader<TileSelected>,
    selected_unit: Option<Res<SelectedUnit>>,
    units: Query<(Entity, &UnitPos)>,
) {
    let selected_unit = selected_unit.map(|unit| **unit);

    // `.last()` bc it's better to eat simultaneous inputs than cause weird bugs
    if let Some(selected_tile) = selected_tiles.iter().last() {
        for (unit, unit_pos) in &units {
            if **unit_pos == **selected_tile {
                // There's a unit on this tile!
                if Some(unit) == selected_unit {
                    // The unit is already selected. Unselect it.
                    commands.remove_resource::<SelectedUnit>();
                } else {
                    commands.insert_resource(SelectedUnit(unit));
                }
            }
        }
    }
}

fn move_unit(
    mut commands: Commands,
    mut selecteds: EventReader<TileSelected>,
    mut move_unit: EventWriter<MoveUnit>,
    units: Query<&UnitPos>,
    selected_unit: Res<SelectedUnit>,
) {
    if let Some(selected) = selecteds.iter().last() {
        if !units.iter().any(|unit_pos| **unit_pos == **selected) {
            // This approach will get really messy once the units have many parts
            move_unit.send(MoveUnit {
                from: **units.get(**selected_unit).unwrap(),
                to: **selected,
            });
            commands.remove_resource::<SelectedUnit>();
        }
    }
}
