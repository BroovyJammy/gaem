use crate::asset::MapAssets;
use crate::gameplay::insect_body::{InsectPart, InsectPartKind, PartDirection};
use crate::map::tile::{Select, SelectTile};
use crate::map::{Layer, LayerToMap};
use crate::map::{MAP_SIZE, TILE_SIZE};
use crate::{gameplay::insect_body::InsectBody, prelude::*};
use leafwing_input_manager::user_input::InputKind;
pub struct GameplayPlugin;

mod insect_body;

impl Plugin for GameplayPlugin {
    fn build(&self, app: &mut App) {
        app.add_system(select_unit.run_in_state(AppState::Game));
        app.add_system(
            move_unit
                .run_in_state(AppState::Game)
                .run_if_resource_exists::<SelectedUnit>(),
        );
        #[derive(SystemLabel)]
        struct TempInsertUnits;
        #[derive(SystemLabel)]
        struct Thingy;

        // Awesome temporary system
        app.add_enter_system(
            AppState::Game,
            (|mut commands: Commands, assets: Res<MapAssets>| {
                for x in 0..MAP_SIZE {
                    for y in 0..MAP_SIZE {
                        if (x + y) % 12 == 0 {
                            let map_size = TilemapSize {
                                x: MAP_SIZE,
                                y: MAP_SIZE,
                            };
                            let grid_size = TilemapGridSize {
                                x: TILE_SIZE as f32,
                                y: TILE_SIZE as f32,
                            };
                            let tile_size = TilemapTileSize {
                                x: TILE_SIZE as f32,
                                y: TILE_SIZE as f32,
                            };

                            commands
                                .spawn()
                                .insert_bundle(TransformBundle { ..default() })
                                .insert(UnitPos(UVec2::new(x, y)))
                                .insert(InsectBody {
                                    parts: Box::new([
                                        InsectPart {
                                            kind: InsectPartKind::Head,
                                            position: (0, 0),
                                            rotation: PartDirection::Down,
                                        },
                                        InsectPart {
                                            kind: InsectPartKind::Flesh,
                                            position: (0, 1),
                                            rotation: PartDirection::Up,
                                        },
                                        InsectPart {
                                            kind: InsectPartKind::Legs,
                                            position: (1, 1),
                                            rotation: PartDirection::Right,
                                        },
                                    ]),
                                    used_tiles: std::collections::HashSet::from([
                                        (0, 0),
                                        (0, 1),
                                        (1, 1),
                                    ]),
                                })
                                .insert_bundle(TilemapBundle {
                                    grid_size,
                                    size: map_size,
                                    storage: TileStorage::empty(map_size),
                                    texture: TilemapTexture(assets.insect.clone()),
                                    tile_size,
                                    transform: Transform::from_translation(Vec3::new(
                                        0.0, 0.0, 0.0,
                                    )),
                                    ..default()
                                });
                        }
                    }
                }
            })
            .label(TempInsertUnits),
        )
        .add_system(
            insect_body::update_insect_body_tilemap
                .run_in_state(AppState::Game)
                .after(TempInsertUnits)
                .label(Thingy),
        )
        .add_system(
            sync_unit_pos_with_transform
                .run_in_state(AppState::Game)
                .after(TempInsertUnits)
                .before(Thingy),
        );
        app.add_event::<ReselectTile>()
            .add_system(
                handle_input
                    .run_in_state(AppState::Game)
                    .label(InputHandlingSystem),
            )
            .add_system(
                camera_to_selected_tile
                    .run_in_state(AppState::Game)
                    .after(InputHandlingSystem),
            )
            .insert_resource(CursorTilePos {
                pos: UVec2::new(20, 20),
                snap_camera_to: true,
            })
            .add_event::<TileSelected>()
            .add_system(
                update_cursor_pos
                    .run_in_state(AppState::Game)
                    .after(InputHandlingSystem)
                    .label("update_cursor_pos"),
            )
            .add_system(
                highlight_hovered_tile
                    .run_in_state(AppState::Game)
                    .after("update_cursor_pos"),
            )
            .add_system(
                select_tile
                    .run_in_state(AppState::Game)
                    .after("update_cursor_pos"),
            );
    }
}

// Resource that only exists when a unit is selected
#[derive(Clone, Copy, Deref)]
struct SelectedUnit(Entity);

#[derive(Component, Deref, Copy, Clone, Debug, Eq, PartialEq)]
pub struct UnitPos(UVec2);

fn select_unit(
    mut commands: Commands,
    mut selected_tiles: EventReader<TileSelected>,
    selected_unit: Option<Res<SelectedUnit>>,
    units: Query<(Entity, &UnitPos, &InsectBody)>,
) {
    let selected_unit = selected_unit.map(|unit| **unit);

    // `.last()` bc it's better to eat simultaneous inputs than cause weird bugs
    if let Some(selected_tile) = selected_tiles.iter().last() {
        for (unit, pos, body) in &units {
            if body.contains_tile(*pos, **selected_tile) {
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
    mut units: Query<(&mut UnitPos, &InsectBody)>,
    selected_unit: Res<SelectedUnit>,
) {
    if let Some(selected) = selecteds.iter().last() {
        if !units
            .iter()
            .any(|(unit_pos, body)| body.contains_tile(*unit_pos, **selected))
        {
            let (mut move_unit_from, _) = units.get_mut(**selected_unit).unwrap();
            let move_unit_to = selected.0;
            move_unit_from.0 = move_unit_to;
            commands.remove_resource::<SelectedUnit>();
        }
    }
}

fn sync_unit_pos_with_transform(mut unit_transform: Query<(&UnitPos, &mut Transform)>) {
    for (unit_pos, mut trans) in unit_transform.iter_mut() {
        trans.translation =
            (Vec2::new(unit_pos.x as f32, unit_pos.y as f32) * TILE_SIZE as f32).extend(1.0);
    }
}

#[derive(Actionlike, PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub enum Action {
    MoveSelection,
    Select,
}

pub struct ReselectTile;

#[derive(Component, Copy, Clone)]
pub struct Player;

#[derive(SystemLabel)]
pub struct InputHandlingSystem;

pub fn make_action_manager() -> InputManagerBundle<Action> {
    InputManagerBundle {
        action_state: ActionState::default(),
        input_map: InputMap::default()
            .insert(DualAxis::left_stick(), Action::MoveSelection)
            .insert(
                VirtualDPad {
                    up: InputKind::GamepadButton(GamepadButtonType::DPadUp),
                    down: InputKind::GamepadButton(GamepadButtonType::DPadDown),
                    left: InputKind::GamepadButton(GamepadButtonType::DPadLeft),
                    right: InputKind::GamepadButton(GamepadButtonType::DPadRight),
                },
                Action::MoveSelection,
            )
            .insert(
                VirtualDPad {
                    up: InputKind::Keyboard(KeyCode::W),
                    down: InputKind::Keyboard(KeyCode::S),
                    left: InputKind::Keyboard(KeyCode::A),
                    right: InputKind::Keyboard(KeyCode::D),
                },
                Action::MoveSelection,
            )
            .insert(
                VirtualDPad {
                    up: InputKind::Keyboard(KeyCode::Up),
                    down: InputKind::Keyboard(KeyCode::Down),
                    left: InputKind::Keyboard(KeyCode::Left),
                    right: InputKind::Keyboard(KeyCode::Right),
                },
                Action::MoveSelection,
            )
            .insert(MouseButton::Left, Action::Select)
            .insert(KeyCode::Space, Action::Select)
            // `South`, meaning A. South on D-Pad is `DPadDown`.
            .insert(GamepadButtonType::South, Action::Select)
            .build(),
    }
}

pub fn handle_input(
    mut camera: Query<&mut Transform, With<Camera>>,
    mut actioners: Query<&ActionState<Action>>,
    mut writer: EventWriter<ReselectTile>,
) {
    const CAM_SPEED: f32 = 4.0;

    let actioner_state = actioners.get_single_mut().unwrap();
    if actioner_state.pressed(Action::MoveSelection) {
        let movement = actioner_state
            .clamped_axis_pair(Action::MoveSelection)
            .unwrap() // no idea when this is None :shrug:
            .xy();
        let mut camera_trans = camera.get_single_mut().unwrap();
        camera_trans.translation += movement.extend(0.0) * CAM_SPEED;
        writer.send(ReselectTile);
    }
}

// this only really runs on startup rn
fn camera_to_selected_tile(
    mut set: ParamSet<(Query<&mut Transform, With<Camera>>, Query<&Transform>)>,
    layer_to_map: Res<LayerToMap>,
    mut selected_tile: ResMut<CursorTilePos>,
) {
    if !selected_tile.snap_camera_to {
        return;
    }
    selected_tile.snap_camera_to = false;

    let map_entity = layer_to_map.0[&Layer::Select];
    let map_trans = *set.p1().get(map_entity).unwrap();
    let selected_tile_pos = map_trans.translation
        + (selected_tile.pos * UVec2::splat(TILE_SIZE))
            .extend(0)
            .as_vec3();

    let mut temp = set.p0();
    let mut camera_trans = temp.get_single_mut().unwrap();
    // FIXME this probably sets `z` unnecessarily and also we should smooth move to the target
    // instead of directly jumping to it.
    camera_trans.translation = selected_tile_pos;
}

// The position of the tile that the cursor is over
// I use `UVec2` instead of `TilePos` bc `UVec2` impls more useful traits
#[derive(Default)]
pub struct CursorTilePos {
    pub pos: UVec2,
    pub snap_camera_to: bool,
}

// Event that talks to the gameplay part
#[derive(Deref)]
pub struct TileSelected(UVec2);

// Adapted from Star Machine, a game that's currently on hold
// Assumes that the map is at the origin
fn update_cursor_pos(
    cameras: Query<&Transform, With<Camera2d>>,
    mut cursor_movement: EventReader<CursorMoved>,
    mut reselect_tile: EventReader<ReselectTile>,
    windows: Res<Windows>,
    mut cursor: ResMut<CursorTilePos>,
) {
    let window = windows.get_primary().unwrap();
    let position = match match cursor_movement.iter().find(|event| event.id == window.id()) {
        Some(_) => window.cursor_position(),
        None => None,
    } {
        Some(a) => Some(a),
        None => match reselect_tile.iter().next() {
            Some(_) => Some(Vec2::new(window.width(), window.height()) / 2.),
            None => return,
        },
    }
    .unwrap();
    let new_pos = cameras.get_single().ok().and_then(|camera_transform| {
        let new_pos = (camera_transform.compute_matrix()
            * (position - Vec2::new(window.width(), window.height()) / 2.)
                .extend(0.)
                .extend(1.))
        .truncate()
        .truncate()
            / TILE_SIZE as f32;

        (new_pos.cmpge(Vec2::ZERO).all()).then(|| new_pos.as_uvec2())
    });
    if let Some(new_pos) = new_pos {
        cursor.pos = new_pos;
    }
}

// Not optimized, but it keeps it simple
pub fn highlight_hovered_tile(
    mut tiles: Query<(&mut TileTexture, &TilePos), With<SelectTile>>,
    cursor_pos: Res<CursorTilePos>,
) {
    for (mut texture, &tile_pos) in &mut tiles {
        let tile_pos: UVec2 = tile_pos.into();
        *texture = match tile_pos == cursor_pos.pos {
            true => Select::Active,
            false => Select::Inactive,
        }
        .into();
    }
}

fn select_tile(
    mut selecteds: EventWriter<TileSelected>,
    actioners: Query<&ActionState<Action>>,
    cursor_pos: Res<CursorTilePos>,
) {
    if actioners.single().just_pressed(Action::Select) {
        selecteds.send(TileSelected(cursor_pos.pos));
    }
}
