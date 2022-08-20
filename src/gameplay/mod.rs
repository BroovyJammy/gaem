use crate::prelude::*;

use crate::{
    map::{TileSelected, MAP_SIZE},
};

pub struct GameplayPlugin;

impl Plugin for GameplayPlugin {
    fn build(&self, app: &mut App) {
        app.add_event::<MoveUnit>();
        app.add_system(select_unit.run_in_state(AppState::Game));
        app.add_system(
            move_unit
                .run_in_state(AppState::Game)
                .run_if_resource_exists::<SelectedUnit>(),
        );
        // Awesome temporary system
        app.add_system(|mut commands: Commands| {
            for x in 0..MAP_SIZE {
                for y in 0..MAP_SIZE {
                    if (x + y) % 12 == 0 {
                        commands.spawn().insert(UnitPos(UVec2::new(x, y)));
                    }
                }
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
    pub from: UVec2,
    pub to: UVec2,
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
