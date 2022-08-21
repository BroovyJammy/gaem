use crate::asset::MapAssets;
use crate::gameplay::insect_body::{InsectPart, InsectPartKind, PartDirection};
use crate::map::tile::{MovementTile, Select, SelectTile};
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
                .run_in_state(Turn::goodie())
                .run_if_resource_exists::<SelectedUnit>(),
        );
        app.add_system(
            highlight_movable_tiles
                .run_in_state(AppState::Game)
                .run_in_state(Turn::goodie())
                .run_if_resource_added::<SelectedUnit>(),
        );
        app.add_system(
            remove_movement_indicator
                .run_in_state(AppState::Game)
                .run_if_resource_removed::<SelectedUnit>(),
        );
        #[derive(SystemLabel)]
        struct TempInsertUnits;
        #[derive(SystemLabel)]
        struct Thingy;

        app.add_enter_system(AppState::Game, insert_units.label(TempInsertUnits))
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
                    .run_in_state(Turn::goodie())
                    .after("update_cursor_pos"),
            );

        app.add_loopless_state(Turn::goodie())
            .add_system(
                end_turn
                    .run_in_state(AppState::Game)
                    .run_in_state(Turn::goodie()),
            )
            .add_system(
                enemy_turn
                    .run_in_state(AppState::Game)
                    .run_in_state(Turn::baddie()),
            );
    }
}

// Resource that only exists when a unit is selected
#[derive(Clone, Copy, Deref)]
struct SelectedUnit(Entity);

#[derive(Component, Deref, Copy, Clone, Debug, Eq, PartialEq)]
pub struct UnitPos(UVec2);

#[derive(Clone, Component, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Team {
    Goodie,
    Baddie,
}

#[derive(Clone, Copy, Debug, Deref, DerefMut, Eq, Hash, PartialEq)]
pub struct Turn(pub Team);

impl Turn {
    fn goodie() -> Self {
        Self(Team::Goodie)
    }

    fn baddie() -> Self {
        Self(Team::Baddie)
    }
}

impl Team {
    fn color(self) -> Color {
        match self {
            Team::Goodie => Color::WHITE,
            // Cyan is the clearest tint, since the units are red-colored
            // Might want to indicate with some other method if this is too ugly
            Team::Baddie => Color::rgb(0.3, 1., 1.),
        }
    }
}

// Temporary (moved to fn since it grew)
fn insert_units(mut commands: Commands, assets: Res<MapAssets>) {
    let mut team = true;
    for x in 0..MAP_SIZE {
        for y in 0..MAP_SIZE {
            if y % 3 != 0 {
                continue;
            }

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

                team = !team;
                let team = match team {
                    true => Team::Goodie,
                    false => Team::Baddie,
                };

                let body = {
                    let src_1 = InsectBody::new(vec![
                        InsectPart::new((0, 0), InsectPartKind::Head, PartDirection::Down),
                        InsectPart::new((0, 1), InsectPartKind::Flesh, PartDirection::Up),
                        InsectPart::new((1, 1), InsectPartKind::Flesh, PartDirection::Right),
                    ]);
                    let src_2 = InsectBody::new(vec![
                        InsectPart::new((0, 0), InsectPartKind::Flesh, PartDirection::Down),
                        InsectPart::new((0, 1), InsectPartKind::Flesh, PartDirection::Up),
                        InsectPart::new((1, 1), InsectPartKind::Legs, PartDirection::Right),
                    ]);
                    insect_body::generate_body(&[src_1, src_2], 2)
                };

                commands
                    .spawn()
                    .insert_bundle(TransformBundle { ..default() })
                    .insert(UnitPos(UVec2::new(x, y)))
                    .insert(body)
                    .insert_bundle(TilemapBundle {
                        grid_size,
                        size: map_size,
                        storage: TileStorage::empty(map_size),
                        texture: TilemapTexture(assets.insect.clone()),
                        tile_size,
                        transform: Transform::from_translation(Vec3::new(0.0, 0.0, 0.0)),
                        ..default()
                    })
                    .insert(team);
            }
        }
    }
}

fn select_unit(
    mut commands: Commands,
    mut selected_tiles: EventReader<TileSelected>,
    selected_unit: Option<Res<SelectedUnit>>,
    units: Query<(Entity, &UnitPos, &InsectBody, &Team)>,
) {
    let selected_unit = selected_unit.map(|unit| **unit);

    // `.last()` bc it's better to eat simultaneous inputs than cause weird bugs
    if let Some(selected_tile) = selected_tiles.iter().last() {
        for (unit, pos, body, team) in &units {
            if body.contains_tile(*pos, **selected_tile) {
                // There's a unit on this tile!
                if *team == Team::Baddie || Some(unit) == selected_unit {
                    // The unit is already selected, or clicked on a baddie. Unselect.
                    commands.remove_resource::<SelectedUnit>();
                } else {
                    commands.insert_resource(SelectedUnit(unit));
                }
            }
        }
    }
}

fn can_move_unit<'a>(
    mover: Entity,
    mover_body: &InsectBody,
    from: UVec2,
    to: UVec2,
    units: impl IntoIterator<Item = (Entity, &'a UnitPos, &'a InsectBody)>,
) -> bool {
    let move_delta = to.as_ivec2() - from.as_ivec2();

    (move_delta.x.abs() + move_delta.y.abs()) as u32 <= mover_body.move_speed()
        && !units.into_iter().any(|(unit_entity, unit_pos, body)| {
            if unit_entity == mover {
                return false;
            }

            mover_body
                .used_tiles
                .iter()
                .any(|(x, y)| body.contains_tile(*unit_pos, UVec2::new(x + to.x, y + to.y)))
        })
}

fn highlight_movable_tiles(
    units: Query<(Entity, &UnitPos, &InsectBody)>,
    mut movement_tiles: Query<(&mut TileTexture, &TilePos), With<MovementTile>>,
    selected_unit: Res<SelectedUnit>,
) {
    let (_, unit_pos, body) = units.get(**selected_unit).unwrap();

    for (mut texture, tile_pos) in &mut movement_tiles {
        if can_move_unit(**selected_unit, body, **unit_pos, tile_pos.into(), &units) {
            *texture = Select::Active.into();
        }
    }
}

fn move_unit(
    mut commands: Commands,
    mut tile_selected: EventReader<TileSelected>,
    mut units: Query<(Entity, &mut UnitPos, &InsectBody)>,
    selected_unit: Res<SelectedUnit>,
) {
    if let Some(selected_tile) = tile_selected.iter().last() {
        let (_, move_unit_from, selected_body) = units.get(selected_unit.0).unwrap();

        if can_move_unit(
            **selected_unit,
            selected_body,
            **move_unit_from,
            **selected_tile,
            &units,
        ) {
            let (_, mut move_unit_from, _) = units.get_mut(**selected_unit).unwrap();
            let move_unit_to = selected_tile.0;
            move_unit_from.0 = move_unit_to;
            commands.remove_resource::<SelectedUnit>();
        }
    }
}

fn remove_movement_indicator(mut movement_tiles: Query<&mut TileTexture, With<MovementTile>>) {
    for mut texture in &mut movement_tiles {
        *texture = Select::Inactive.into();
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
    EndTurn,
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
            .insert(KeyCode::Return, Action::EndTurn)
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

fn end_turn(mut commands: Commands, actioners: Query<&ActionState<Action>>) {
    if actioners.single().just_pressed(Action::EndTurn) {
        commands.remove_resource::<SelectedUnit>();
        commands.insert_resource(NextState(Turn::baddie()));
    }
}

#[derive(Default, Deref, DerefMut)]
struct UselessTimer(Duration);

fn enemy_turn(mut commands: Commands, mut useless_timer: Local<UselessTimer>, time: Res<Time>) {
    // Nothing to do yet
    if **useless_timer > Duration::from_secs(1) {
        **useless_timer = default();
        commands.insert_resource(NextState(Turn::goodie()));
    } else {
        **useless_timer += time.delta();
    }
}
