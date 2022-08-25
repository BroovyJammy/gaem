use std::marker::PhantomData;

use crate::asset::{BodyParts, Terrain, TerrainDescriptor};
use crate::cutscene::CurrentCutscene;
use crate::gameplay::insect_body::{InsectPart, InsectPartKind, PartDirection};
use crate::{gameplay::insect_body::InsectBody, prelude::*};
use bevy::ecs::system::SystemParam;
use bevy::utils::{HashSet, StableHashMap};
use leafwing_input_manager::user_input::InputKind;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

pub mod insect_body;
use insect_body::{spawn_insect, UpdateBody};
pub mod map;
use map::{
    Layer, LayerToMap, MovementTile, Select, SelectTile, TerrainKind, TerrainTile, TILE_SIZE,
};

use self::insect_body::InsectRenderEntities;
pub mod pathy;

pub struct CurrentUnits(Vec<InsectBody>);

#[derive(SystemParam)]
pub struct LevelInfo<'w, 's> {
    pub levels: Res<'w, Levels>,
    pub current_level: Res<'w, CurrentLevel>,
    #[system_param(ignore)]
    _p: PhantomData<&'s ()>,
}

impl LevelInfo<'_, '_> {
    pub fn level(&self) -> &Level {
        &self.levels[self.current_level.0]
    }
}

pub struct Levels(pub Vec<Level>);
impl std::ops::Index<usize> for Levels {
    type Output = Level;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

pub struct Level {
    pub size_x: u32,
    pub size_y: u32,
    pub post_cutscene: Option<String>,
    pub player_spawn_points: Vec<UVec2>,
    pub enemy_spawn_points: Vec<(UVec2, u8)>,
}

pub struct CurrentLevel(pub usize);

pub struct GameplayPlugin;

impl Plugin for GameplayPlugin {
    fn build(&self, app: &mut App) {
        app.add_startup_system(init_global_camera);
        app.add_system(
            highlight_movable_tiles
                .run_in_state(AppState::Game)
                .run_in_state(Turn::goodie()),
        );
        app.add_system(
            remove_movement_indicator
                .run_in_state(AppState::Game)
                .run_if_resource_removed::<SelectedUnit>(),
        );
        app.add_system(
            spawn_ghost
                .run_in_state(AppState::Game)
                .run_in_state(Turn::goodie())
                .run_if_resource_exists::<SelectedUnit>(),
        );
        app.add_system(
            move_ghost
                .run_in_state(AppState::Game)
                .run_in_state(Turn::goodie())
                .run_if_resource_exists::<SelectedUnit>(),
        );
        app.add_system(despawn_ghost.run_in_state(AppState::Game));

        #[derive(SystemLabel)]
        struct Thingy;

        app.add_enter_system(AppState::Game, insert_units)
            .add_enter_system(AppState::Game, map::init_map)
            .add_system(
                insect_body::update_insect_body_tilemap
                    .run_in_state(AppState::Game)
                    .label(Thingy),
            )
            .add_system(
                sync_unit_pos_with_transform
                    .run_in_state(AppState::Game)
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
            .add_enter_system(AppState::Game, |mut cmds: Commands| {
                cmds.insert_resource(CursorTilePos {
                    pos: IVec2::new(20, 20),
                    snap_camera_to: true,
                })
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
                handle_select_action
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
                move_enemy_unit
                    .run_in_state(AppState::Game)
                    .run_in_state(Turn::baddie()),
            )
            .add_exit_system(Turn::goodie(), attack(Team::Goodie))
            .add_exit_system(Turn::baddie(), attack(Team::Baddie))
            .add_exit_system(Turn::goodie(), replenish_move_cap(Team::Baddie))
            .add_exit_system(Turn::baddie(), replenish_move_cap(Team::Goodie))
            .add_enter_system(Turn::baddie(), mark_movables)
            .add_system(
                lose_win_conditions
                    .run_in_state(AppState::Game)
                    .run_in_state(Turn::goodie()),
            )
            .add_exit_system(AppState::Game, cleanup_stuff);
    }
}

fn body_sources(stats: &BodyParts) -> (InsectBody, InsectBody, InsectBody) {
    let src_1 = InsectBody::new(vec![
        InsectPart::new((0, 0), InsectPartKind(1), PartDirection::Down, &stats),
        InsectPart::new((0, 1), InsectPartKind(3), PartDirection::Up, &stats),
        InsectPart::new((1, 1), InsectPartKind(0), PartDirection::Right, &stats),
    ]);
    let src_2 = InsectBody::new(vec![
        InsectPart::new((0, 0), InsectPartKind(3), PartDirection::Down, &stats),
        InsectPart::new((0, 1), InsectPartKind(0), PartDirection::Up, &stats),
        InsectPart::new((1, 1), InsectPartKind(2), PartDirection::Right, &stats),
    ]);
    let src_3 = InsectBody::new(vec![
        InsectPart::new((0, 1), InsectPartKind(5), PartDirection::Left, &stats),
        InsectPart::new((1, 1), InsectPartKind(7), PartDirection::Up, &stats),
        InsectPart::new((1, 0), InsectPartKind(6), PartDirection::Down, &stats),
        InsectPart::new((1, 2), InsectPartKind(4), PartDirection::Right, &stats),
    ]);
    (src_1, src_2, src_3)
}

fn init_global_camera(mut commands: Commands) {
    commands
        .spawn_bundle(Camera2dBundle::default())
        .insert_bundle(make_action_manager());
}

// Resource that only exists when a unit is selected
#[derive(Clone, Copy)]
struct SelectedUnit {
    unit: Entity,
    /// Records which tile `unit` was clicked on at
    at_local_pos: UVec2,
}

#[derive(Component, Deref, DerefMut, Copy, Clone, Debug, Eq, PartialEq)]
pub struct UnitPos(IVec2);

#[derive(Clone, Component, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Team {
    Goodie,
    Baddie,
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

#[derive(Component)]
#[component(storage = "SparseSet")]
struct MoveMe;

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

#[derive(Component, Copy, Clone)]
pub struct MoveCap(u32);

// The position of the tile that the cursor is over
// I use `UVec2` instead of `TilePos` bc `UVec2` impls more useful traits
#[derive(Default)]
pub struct CursorTilePos {
    pub pos: IVec2,
    pub snap_camera_to: bool,
}

// Event that talks to the gameplay part
#[derive(Deref)]
pub struct TileSelected(IVec2);

#[derive(SystemParam)]
pub struct TerrainInfo<'w, 's> {
    pub tiles: Query<'w, 's, (&'static TilePos, &'static TerrainKind)>,
    pub tilestorages: Query<'w, 's, &'static TileStorage>,
    pub layer_to_map: Res<'w, LayerToMap>,
    pub terrain: Res<'w, Terrain>,
    pub level_info: LevelInfo<'w, 's>,
}

impl TerrainInfo<'_, '_> {
    pub fn get_terrain_map(&self) -> &TileStorage {
        self.tilestorages
            .get(self.layer_to_map[&Layer::Terrain])
            .unwrap()
    }

    pub fn get_terrain_descriptor<'a>(
        &'a self,
        terrain_map: &'a TileStorage,
    ) -> impl Fn(UnitPos) -> Option<(TerrainKind, &'a TerrainDescriptor)> + Copy + 'a {
        move |pos| {
            if pos.0.x < 0
                || pos.0.x >= self.level_info.level().size_x as i32
                || pos.0.y < 0
                || pos.0.y >= self.level_info.level().size_y as i32
            {
                return None;
            }
            let terrain_entity = terrain_map
                .get(&TilePos::new(pos.0.x as u32, pos.0.y as u32))
                .unwrap();
            let terrain_kind = self.tiles.get(terrain_entity).unwrap().1.clone();
            Some((terrain_kind, &self.terrain[terrain_kind]))
        }
    }
}

// Temporary (moved to fn since it grew)
fn insert_units(
    mut commands: Commands,
    stats: Res<BodyParts>,
    level_info: LevelInfo<'_, '_>,
    player_units: Option<Res<CurrentUnits>>,
) {
    let mut seeds = vec![];
    let (src_1, src_2, src_3) = body_sources(&stats);

    let player_units = match player_units {
        None => {
            let seed = seeds.pop().unwrap_or_else(|| rand::thread_rng().gen());
            debug!("seed: {}", seed);
            let (Ok(body) | Err(body)) = insect_body::generate_body(
                &[src_1.clone(), src_2.clone(), src_3.clone()],
                2,
                &mut StdRng::seed_from_u64(seed),
                &stats,
            );
            commands.insert_resource(CurrentUnits(vec![body.clone()]));
            CurrentUnits(vec![body])
        }
        Some(current_units) => CurrentUnits(current_units.0.clone()),
    };

    let level = level_info.level();
    for (pos, generation) in level.enemy_spawn_points.iter().copied() {
        let seed = seeds.pop().unwrap_or_else(|| rand::thread_rng().gen());
        debug!("seed: {}", seed);
        let (Ok(body) | Err(body)) = insect_body::generate_body(
            &[src_1.clone(), src_2.clone(), src_3.clone()],
            generation,
            &mut StdRng::seed_from_u64(seed),
            &stats,
        );
        let move_cap = MoveCap(body.max_move_cap(&stats));
        spawn_insect(&mut commands, pos.as_ivec2(), body, Team::Baddie, move_cap);
    }

    assert!(level.player_spawn_points.len() >= player_units.0.len());
    for (pos, body) in level
        .player_spawn_points
        .iter()
        .copied()
        .zip(player_units.0.iter().cloned())
    {
        let move_cap = MoveCap(body.max_move_cap(&stats));
        spawn_insect(&mut commands, pos.as_ivec2(), body, Team::Goodie, move_cap);
    }
}

fn handle_select_action(
    mut commands: Commands<'_, '_>,
    mut tile_selected_writer: EventWriter<TileSelected>,
    actioners: Query<&ActionState<Action>>,
    cursor_pos: Res<CursorTilePos>,
    selected_unit: Option<Res<SelectedUnit>>,
    mut units: Query<(Entity, &mut UnitPos, &InsectBody, &Team, &mut MoveCap)>,
    terrain_info: TerrainInfo<'_, '_>,
) {
    if !actioners.single().just_pressed(Action::Select) {
        return;
    }
    tile_selected_writer.send(TileSelected(cursor_pos.pos));

    for (unit, pos, body, team, _) in &units {
        if body.contains_tile(*pos, cursor_pos.pos) {
            match team {
                Team::Goodie => commands.insert_resource(SelectedUnit {
                    unit,
                    at_local_pos: (cursor_pos.pos - pos.0).as_uvec2(),
                }),
                Team::Baddie => commands.remove_resource::<SelectedUnit>(),
            }
            return;
        }
    }

    if let Some(selected_unit) = selected_unit {
        let (_, move_unit_from, selected_body, _, move_cap) =
            units.get(selected_unit.unit).unwrap();

        let moveable_to_tiles = pathy::get_movable_to_tiles(
            selected_unit.unit,
            move_unit_from.0,
            selected_body,
            *move_cap,
            || units.iter().map(|(a, b, c, _, _)| (a, b, c)),
            &terrain_info,
        );

        let (_, mut unit_pos, _, _, mut move_cap) = units.get_mut(selected_unit.unit).unwrap();
        let move_unit_to = cursor_pos.pos - selected_unit.at_local_pos.as_ivec2();

        if move_unit_to.x >= 0
            && move_unit_to.y >= 0
            && moveable_to_tiles.contains(&move_unit_to.as_uvec2())
        {
            let move_delta = unit_pos.0 - move_unit_to;
            move_cap.0 -= (move_delta.x.abs() + move_delta.y.abs()) as u32;
            unit_pos.0 = move_unit_to;
            commands.remove_resource::<SelectedUnit>();
        }
    }
}

fn highlight_movable_tiles(
    units: Query<(Entity, &UnitPos, &InsectBody, &MoveCap)>,
    mut movement_tiles: Query<(&mut TileTexture, &TilePos), With<MovementTile>>,
    selected_unit: Option<Res<SelectedUnit>>,
    terrain_info: TerrainInfo<'_, '_>,
) {
    let selected_unit = match selected_unit {
        Some(unit) if unit.is_changed() => unit,
        _ => return,
    };

    for (mut texture, _) in &mut movement_tiles {
        *texture = Select::Inactive.into();
    }

    let (_, unit_pos, body, move_cap) = units.get(selected_unit.unit).unwrap();
    let moveable_to_tiles = pathy::get_movable_to_tiles(
        selected_unit.unit,
        unit_pos.0,
        body,
        *move_cap,
        || units.iter().map(|(a, b, c, _)| (a, b, c)),
        &terrain_info,
    );
    for &tile in moveable_to_tiles.iter() {
        let movement_tile_storage = terrain_info
            .tilestorages
            .get(terrain_info.layer_to_map[&Layer::Movement])
            .unwrap();
        let tile = movement_tile_storage
            .get(&TilePos::new(
                tile.x + selected_unit.at_local_pos.x,
                tile.y + selected_unit.at_local_pos.y,
            ))
            .unwrap();
        *movement_tiles.get_mut(tile).unwrap().0 = Select::Active.into();
    }
}

#[derive(Component)]
pub struct Ghost;

// A ghost is the unit that shows under your cursor when you have a unit selected
// 👻
fn spawn_ghost(
    mut commands: Commands,
    units: Query<(&UnitPos, &InsectBody)>,
    selected_unit: Res<SelectedUnit>,
) {
    if selected_unit.is_changed() {
        let (pos, body) = units.get(selected_unit.unit).unwrap();
        commands
            .spawn()
            .insert_bundle(TransformBundle::default())
            .insert(*pos)
            .insert(body.clone())
            .insert(UpdateBody)
            .insert(Team::Goodie)
            .insert(InsectRenderEntities {
                hp_bar: StableHashMap::with_hasher(default()),
                body_part: StableHashMap::with_hasher(default()),
            })
            .insert_bundle(VisibilityBundle::default())
            .insert(Ghost);
    }
}

// Causes the ghost to haunt the cursor
fn move_ghost(
    mut ghosts: Query<&mut UnitPos, With<Ghost>>,
    selected_unit: Res<SelectedUnit>,
    cursor_pos: Res<CursorTilePos>,
) {
    if !cursor_pos.is_changed() {
        return;
    }

    **match ghosts.get_single_mut() {
        Ok(ghost) => ghost,
        Err(_) => return,
    } = cursor_pos.pos - selected_unit.at_local_pos.as_ivec2();
}

fn despawn_ghost(
    mut commands: Commands,
    selected_unit: Option<Res<SelectedUnit>>,
    ghosts: Query<Entity, With<Ghost>>,
) {
    if selected_unit
        .map(|selected_unit| selected_unit.is_changed())
        .unwrap_or(true)
    {
        for ghost in &ghosts {
            commands.entity(ghost).despawn_recursive();
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
            .insert(GamepadButtonType::Select, Action::EndTurn)
            .insert(GamepadButtonType::North, Action::EndTurn)
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
        camera_trans.translation = camera_trans.translation.as_ivec3().as_vec3();
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
        + (selected_tile.pos * IVec2::splat(TILE_SIZE as i32))
            .extend(0)
            .as_vec3();

    let mut temp = set.p0();
    let mut camera_trans = temp.get_single_mut().unwrap();
    // FIXME this probably sets `z` unnecessarily and also we should smooth move to the target
    // instead of directly jumping to it.
    camera_trans.translation = selected_tile_pos;
}

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

        (new_pos.cmpge(Vec2::ZERO).all()).then(|| new_pos.as_ivec2())
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
        let tile_pos = UVec2::as_ivec2(&tile_pos.into());
        *texture = match tile_pos == cursor_pos.pos {
            true => Select::Active,
            false => Select::Inactive,
        }
        .into();
    }
}

fn end_turn(mut commands: Commands, actioners: Query<&ActionState<Action>>) {
    if actioners.single().just_pressed(Action::EndTurn) {
        commands.remove_resource::<SelectedUnit>();
        commands.insert_resource(NextState(Turn::baddie()));
    }
}

fn cleanup_stuff(
    mut commands: Commands<'_, '_>,
    to_despawn: Query<
        Entity,
        Or<(
            With<Team>,
            With<TileStorage>,
            With<SelectTile>,
            With<TerrainTile>,
            With<MovementTile>,
        )>,
    >,
) {
    for e in to_despawn.iter() {
        commands.entity(e).despawn_recursive();
    }
}

fn lose_win_conditions(
    mut commands: Commands<'_, '_>,
    units: Query<(Entity, &Team)>,
    levels: Res<Levels>,
    mut current_level: ResMut<CurrentLevel>,
) {
    if units
        .iter()
        .filter(|(_, team)| matches!(team, Team::Baddie))
        .next()
        .is_none()
    {
        let level = &levels[current_level.0];
        if let Some(cutscene) = &level.post_cutscene {
            commands.insert_resource(CurrentCutscene::new(&cutscene));
            commands.insert_resource(NextState(AppState::PlayCutscene));
            current_level.0 += 1;
        } else {
            debug!("finished level but there was no cutscene to move to");
        }
    }

    if units
        .iter()
        .filter(|(_, team)| matches!(team, Team::Goodie))
        .next()
        .is_none()
    {
        *current_level = CurrentLevel(0);
        commands.remove_resource::<CurrentUnits>();
        commands.insert_resource(NextState(AppState::MainMenu));
    }
}

fn mark_movables(mut commands: Commands, units: Query<(Entity, &Team)>) {
    for (unit, team) in &units {
        if *team == Team::Baddie {
            commands.entity(unit).insert(MoveMe);
        }
    }
}

// AI might be too hard lol
fn move_enemy_unit(
    mut commands: Commands,
    move_me: Query<Entity, With<MoveMe>>,
    mut units: Query<(Entity, &mut UnitPos, &InsectBody, &MoveCap, &Team)>,
    movement_tiles: Query<&TilePos, With<MovementTile>>,
    terrain_info: TerrainInfo<'_, '_>,
    stats: Res<BodyParts>,
) {
    if let Some((unit, unit_pos, body, move_cap, _)) =
        move_me.iter().next().map(|unit| units.get(unit).unwrap())
    {
        // AI tries to deal the most damage per turn
        let mut best_dests = Vec::new();
        let mut best_score = i32::MIN;
        let mut best_attacking = false;

        let unit_size = UVec2::from(
            body.used_tiles
                .iter()
                .fold((0, 0), |(max_x, max_y), (x, y)| {
                    (max_x.max(*x), max_y.max(*y))
                }),
        );

        let unit_sizes = units
            .iter()
            .map(|(other, _, other_body, _, _)| {
                (
                    other,
                    UVec2::from(
                        other_body
                            .used_tiles
                            .iter()
                            .fold((0, 0), |(max_x, max_y), (x, y)| {
                                (max_x.max(*x), max_y.max(*y))
                            }),
                    ),
                )
            })
            .collect::<HashMap<_, _>>();

        // Optimization! Let's only look at units that could possibly be close enough
        let nearby_units = units
            .iter()
            .filter_map(|(other, other_pos, _, _, _)| {
                let other_size = *unit_sizes.get(&other).unwrap();

                let delta = **unit_pos - **other_pos;
                UVec2::new(delta.x.unsigned_abs(), delta.y.unsigned_abs())
                    .cmple(unit_size + other_size + UVec2::splat(move_cap.0))
                    .all()
                    .then(|| other)
            })
            .collect::<Vec<_>>();
        debug!("nearby_units: {:?}", &nearby_units);

        let moveable_to_tiles = pathy::get_movable_to_tiles(
            unit,
            unit_pos.0,
            body,
            *move_cap,
            || {
                nearby_units
                    .iter()
                    .map(|unit| units.get(*unit).map(|(a, b, c, _, _)| (a, b, c)).unwrap())
            },
            &terrain_info,
        );
        debug!("unit pos: {:?}", unit_pos);
        for &tile_pos in moveable_to_tiles.iter() {
            let mut score = 0;
            let mut attacking = false;
            for (other, other_pos, other_body, _, other_team) in
                nearby_units.iter().map(|unit| units.get(*unit).unwrap())
            {
                if unit == other {
                    // Doesn't really matter mathematically,
                    // but should make the scores make more sense during debug
                    continue;
                }

                let other_size = *unit_sizes.get(&other).unwrap();

                let delta = IVec2::new(tile_pos.x as i32, tile_pos.y as i32) - **other_pos;
                if UVec2::new(delta.x.unsigned_abs(), delta.y.unsigned_abs())
                    .cmpgt(unit_size + other_size)
                    .any()
                {
                    // Too far to influence score
                    continue;
                }

                for part in &body.parts {
                    for other_part in &other_body.parts {
                        let delta = (UVec2::new(tile_pos.x, tile_pos.y)
                            + UVec2::from(part.position))
                        .as_ivec2()
                            - (**other_pos + UVec2::from(other_part.position).as_ivec2());

                        if delta.x.abs() + delta.y.abs() == 1 {
                            // Adjacent
                            attacking |= *other_team == Team::Goodie && stats[part.kind].damage > 0;
                            score += stats[part.kind].damage as i32
                                * ((*other_team == Team::Goodie) as i32 * 3 - 1)
                                - stats[other_part.kind].damage as i32;
                        }
                    }
                }
            }

            // Either have to have a better score, or find a place where they can attack
            if (score > best_score && attacking == best_attacking) || (attacking && !best_attacking)
            {
                debug!(
                    "updated best score from: {} {} to: {} {}",
                    best_score,
                    match best_attacking {
                        false => "not attacking",
                        true => "attacking",
                    },
                    score,
                    match attacking {
                        false => "not attacking",
                        true => "attacking",
                    }
                );
                best_dests.clear();
                best_dests.push(UVec2::new(tile_pos.x, tile_pos.y).as_ivec2());
                best_score = score;
                best_attacking = attacking;
            } else if score == best_score {
                best_dests.push(UVec2::new(tile_pos.x, tile_pos.y).as_ivec2());
            }
        }
        // debug!("destinations: {:?}", &best_dests);

        let (_, mut unit_pos, _, _, _) = units.get_mut(unit).unwrap();

        let pos = if best_dests.len() > 0 {
            best_dests[rand::thread_rng().gen_range(0..best_dests.len())]
        } else if moveable_to_tiles.len() > 0 {
            moveable_to_tiles
                .iter()
                .nth(rand::thread_rng().gen_range(0..moveable_to_tiles.len()))
                .unwrap()
                .as_ivec2()
        } else {
            debug!("no movable to tiles");
            unit_pos.0
        };
        **unit_pos = pos;
        commands.entity(unit).remove::<MoveMe>();
    } else {
        // Everyone has moved
        commands.insert_resource(NextState(Turn::goodie()));
    }
}

fn attack(
    attacking_team: Team,
) -> impl Fn(Commands, Query<(Entity, &UnitPos, &mut InsectBody, &Team)>, Res<BodyParts>) {
    move |mut commands, mut units, stats| {
        let mut attacks = Vec::default();

        // avoid using `iter_combinations` because it wont yield both `(enemy, player)` and `(player, enemy)`
        for (attacker, attacker_pos, attacker_body, attacker_team) in units.iter() {
            for (defender, defender_pos, defender_body, _) in units.iter() {
                if defender == attacker || attacking_team != *attacker_team {
                    continue;
                }

                for attacker_part in attacker_body.parts.iter() {
                    let damage = stats[attacker_part.kind].damage;

                    if damage == 0 {
                        continue;
                    }

                    for defender_part in defender_body.parts.iter() {
                        let global_attacker_part_pos =
                            attacker_pos.0 + UVec2::from(attacker_part.position).as_ivec2();
                        let global_defender_part_pos =
                            defender_pos.0 + UVec2::from(defender_part.position).as_ivec2();
                        let delta = global_defender_part_pos - global_attacker_part_pos;

                        if delta.x.abs() + delta.y.abs() == 1 {
                            // Adjacent
                            attacks.push((defender, defender_part.position, damage));
                        }
                    }
                }
            }
        }

        let mut to_sever = HashSet::default();
        let mut severees = HashSet::default();
        for (victim, part_pos, damage) in attacks.into_iter() {
            commands.entity(victim).insert(UpdateBody);

            let (_, _, mut body, _) = units.get_mut(victim).unwrap();
            let part = body.get_part_mut(part_pos).unwrap();

            part.health = part.health.saturating_sub(damage);
            if part.health == 0 {
                // Sometimes the same part receives multiple killing blows,
                // so we have to avoid severing it twice
                to_sever.insert((victim, part_pos));
                severees.insert(victim);
            }
        }

        for (severee, part) in to_sever.into_iter() {
            let (_, _, mut body, _) = units.get_mut(severee).unwrap();
            body.remove_part(part);
        }

        // Have to do this logic separately in case multiple parts were severed
        for severee in severees.into_iter() {
            let (_, body_pos, body, body_team) = units.get(severee).unwrap();

            let mut living = HashSet::<(u32, u32)>::default();
            let mut to_visit = body
                .parts
                .iter()
                .filter_map(|part| stats[part.kind].base.then(|| part.position))
                .enumerate()
                .collect::<Vec<_>>();
            let mut clusters = (0..to_visit.len())
                .map(|cluster| (cluster, default()))
                .collect::<HashMap<usize, HashSet<(u32, u32)>>>();

            while let Some((cluster_i, visit_pos)) = to_visit.pop() {
                if living.contains(&visit_pos) {
                    // Ok this part has already been visited...
                    if !clusters.get(&cluster_i).unwrap().contains(&visit_pos) {
                        // ...but it hasn't been visited in this cluster!
                        // So the clusters should be merged!

                        let cluster = clusters.remove(&cluster_i).unwrap();
                        let (other_i, other) = clusters
                            .iter_mut()
                            .find(|(_, other)| other.contains(&visit_pos))
                            .unwrap();
                        other.extend(cluster.into_iter());

                        // Let's avoid visiting a deleted cluster
                        for (visit_i, _) in to_visit.iter_mut() {
                            if *visit_i == cluster_i {
                                *visit_i = *other_i;
                            }
                        }
                    }

                    continue;
                }

                living.insert(visit_pos);
                clusters.get_mut(&cluster_i).unwrap().insert(visit_pos);

                let part = body.get_part(visit_pos).unwrap();
                for delta in stats[part.kind]
                    .connections
                    .iter()
                    .map(|connection| part.rotation.rotate_ivec(*connection))
                {
                    let next_pos = (visit_pos.0 as i32 + delta.x, visit_pos.1 as i32 + delta.y);
                    if next_pos.0 < 0 || next_pos.1 < 0 {
                        continue;
                    }

                    let next_pos = (next_pos.0 as u32, next_pos.1 as u32);
                    if !body.used_tiles.contains(&next_pos) {
                        continue;
                    }

                    let next_part = body.get_part(next_pos).unwrap();
                    if stats[next_part.kind]
                        .connections
                        .iter()
                        .map(|connection| next_part.rotation.rotate_ivec(*connection))
                        .any(|connection| {
                            UVec2::from(next_pos).as_ivec2() + connection
                                == UVec2::from(visit_pos).as_ivec2()
                        })
                    {
                        to_visit.push((cluster_i, next_pos));
                    }
                }
            }

            for (_, parts) in clusters {
                if parts.is_empty() {
                    continue;
                }

                let body = InsectBody::new(
                    parts
                        .into_iter()
                        .map(|part_pos| *body.get_part(part_pos).unwrap())
                        .collect(),
                );

                let move_cap = body.max_move_cap(&stats);
                spawn_insect(
                    &mut commands,
                    **body_pos,
                    body,
                    *body_team,
                    MoveCap(move_cap),
                );
            }

            commands.entity(severee).despawn_recursive();
        }
    }
}

pub fn replenish_move_cap(
    team: Team,
) -> impl Fn(Query<(&Team, &InsectBody, &mut MoveCap)>, Res<BodyParts>) {
    move |mut query, stats| {
        for (unit_team, body, mut move_cap) in query.iter_mut() {
            if *unit_team == team {
                move_cap.0 = body.max_move_cap(&stats);
            }
        }
    }
}
