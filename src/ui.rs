use bevy::ui::FocusPolicy;

use crate::{
    asset::{BodyParts, UiAssets, UiScenes},
    gameplay::{HoveredInsectPart, SelectedUnit, Squish, Turn},
    prelude::*,
};

pub struct UiPlugin;

impl Plugin for UiPlugin {
    fn build(&self, app: &mut App) {
        app.add_enter_system(AppState::MainMenu, setup_main_menu);
        app.add_exit_system(AppState::MainMenu, cleanup_main_menu);
        app.add_system_to_stage(
            "fuckstages",
            setup_ui_blueprints.run_not_in_state(AppState::AssetsLoading),
        );
        app.add_system(butts_visuals);
        // workaround until we figure out how these things are supposed to be in bevy
        app.register_type::<smallvec::SmallVec<[bevy::ecs::entity::Entity; 8]>>();
        // app.register_type::<Vec<TextSection>>();
        app.register_type::<TextProps>();
        app.register_type::<TextPurpose>();
        app.register_type::<InteractColors>();
        // all the butts
        app.register_type::<butts::ButtExit>();
        app.register_type::<butts::ButtTransition<AppState>>();
        app.add_system(butts_interaction.chain(butts::handle_butt_exit));
        app.add_system(butts_interaction.chain(butts::handle_butt_transition::<AppState>));
        app.add_enter_system(AppState::Game, spawn_sidebar);
        app.add_exit_system(AppState::Game, despawn_combat_ui);
        app.add_system_to_stage(
            "fuckstages",
            add_sidebar_font.run_not_in_state(AppState::AssetsLoading),
        );
        app.add_system(update_sidebar.run_in_state(AppState::Game));
        app.add_enter_system(AppState::Game, spawn_end_turn_button);
        app.add_system(handle_end_turn_button.run_in_state(Turn::input_goodie()));
    }
}

/// We use this instead of `Text` in blueprint scenes
#[derive(Component, Clone, Default, Debug, Reflect, FromReflect)]
#[reflect(Component)]
pub struct TextProps {
    pub value: String,
    pub purpose: TextPurpose,
}

#[derive(Default, Clone, Copy, Debug, Reflect, FromReflect)]
pub enum TextPurpose {
    #[default]
    Unknown,
    Heading,
    Subheading,
    ButtonText,
    GameUi,
    GameValue,
    Dialogue,
    Notification,
}

impl TextProps {
    fn as_text(&self, uiass: &UiAssets) -> Text {
        Text::from_section(&self.value, self.purpose.into_text_style(uiass))
    }
}

impl TextPurpose {
    fn into_text_style(self, uiass: &UiAssets) -> TextStyle {
        match self {
            TextPurpose::Unknown => TextStyle {
                color: Color::PINK,
                font: uiass.font_light.clone(),
                font_size: 8.0,
            },
            TextPurpose::Heading => TextStyle {
                color: Color::ANTIQUE_WHITE,
                font: uiass.font_bold.clone(),
                font_size: 24.0,
            },
            TextPurpose::Subheading => TextStyle {
                color: Color::ANTIQUE_WHITE,
                font: uiass.font_bold.clone(),
                font_size: 18.0,
            },
            TextPurpose::ButtonText => TextStyle {
                color: Color::WHITE,
                font: uiass.font_regular.clone(),
                font_size: 16.0,
            },
            TextPurpose::GameUi => TextStyle {
                color: Color::WHITE,
                font: uiass.font_regular.clone(),
                font_size: 12.0,
            },
            TextPurpose::GameValue => TextStyle {
                color: Color::ALICE_BLUE,
                font: uiass.font_bold.clone(),
                font_size: 14.0,
            },
            TextPurpose::Dialogue => TextStyle {
                color: Color::ALICE_BLUE,
                font: uiass.font_regular.clone(),
                font_size: 12.0,
            },
            TextPurpose::Notification => TextStyle {
                color: Color::ALICE_BLUE,
                font: uiass.font_light.clone(),
                font_size: 12.0,
            },
        }
    }
}

fn setup_ui_blueprints(
    mut commands: Commands,
    q: Query<Entity, (Added<Style>, Without<Node>)>,
    q_btn: Query<Entity, (Added<Button>, Without<Interaction>)>,
    q_text: Query<(Entity, &TextProps), Changed<TextProps>>,
    ui_assets: Res<UiAssets>,
) {
    for e in q.iter() {
        commands
            .entity(e)
            .insert_bundle(SpatialBundle::default())
            .insert(Node::default());
    }
    for e in q_btn.iter() {
        commands.entity(e).insert(Interaction::default());
    }
    for (e, textprops) in q_text.iter() {
        commands
            .entity(e)
            .insert(textprops.as_text(&*ui_assets))
            .insert(CalculatedSize::default());
    }
}

fn setup_main_menu(
    // mut commands: Commands,
    // mut export_event: EventWriter<SceneExportEvent>,
    ui_scenes: Res<UiScenes>,
    mut scene_spawner: ResMut<SceneSpawner>,
) {
    scene_spawner.spawn_dynamic(ui_scenes.main_menu.clone());
    /*
    export_event.send(SceneExportEvent {
        kind: SceneExportKind::Ui,
        path: "assets/scene/main_menu.scn.ron".into(),
    });
    */
}

fn cleanup_main_menu(mut scene_spawner: ResMut<SceneSpawner>, ui_scenes: Res<UiScenes>) {
    scene_spawner.despawn(ui_scenes.main_menu.clone());
}

#[derive(Component, Clone, Default, Debug, Reflect, FromReflect)]
#[reflect(Component)]
pub struct InteractColors {
    disabled: Color,
    inactive: Color,
    hover: Color,
}

fn butts_visuals(
    mut q_color: Query<
        (&Interaction, &InteractColors, &mut UiColor),
        (Changed<Interaction>, With<Button>),
    >,
) {
    for (interaction, colors, mut uicolor) in q_color.iter_mut() {
        let color = match interaction {
            Interaction::None => colors.inactive,
            Interaction::Hovered => colors.hover,
            Interaction::Clicked => colors.hover,
        };
        *uicolor = UiColor(color);
    }
}

fn butts_interaction<B: Component + Clone>(
    mut squishes: EventWriter<Squish>,
    q: Query<(&Interaction, &B), (Changed<Interaction>, With<Button>)>,
) -> Option<B> {
    for (interaction, b) in q.iter() {
        if let Interaction::Clicked = interaction {
            squishes.send(Squish);
            return Some(b.clone());
        }
    }
    None
}

mod butts {
    use crate::prelude::*;
    use bevy::{app::AppExit, ecs::schedule::StateData};

    #[derive(Component, Clone, Default, Reflect, FromReflect)]
    #[reflect(Component)]
    pub struct ButtTransition<T: StateData + FromReflect + Default> {
        state: T,
    }

    pub fn handle_butt_transition<T: StateData + FromReflect + Default>(
        In(butt): In<Option<ButtTransition<T>>>,
        mut commands: Commands,
    ) {
        if let Some(butt) = butt {
            commands.insert_resource(NextState(butt.state));
        }
    }

    #[derive(Component, Clone, Default, Reflect, FromReflect)]
    #[reflect(Component)]
    pub struct ButtExit;

    pub fn handle_butt_exit(In(butt): In<Option<ButtExit>>, mut evw: EventWriter<AppExit>) {
        if butt.is_some() {
            evw.send(AppExit);
        }
    }
}

// Ok it's not really a sidebar anymore. Cornerbar?
#[derive(Component)]
struct Sidebar;

#[derive(Component, FromReflect, Reflect)]
pub struct SidebarName;

#[derive(Component, FromReflect, Reflect)]
pub struct SidebarHealth;

#[derive(Component, FromReflect, Reflect)]
pub struct SidebarDamage;

#[derive(Component, FromReflect, Reflect)]
pub struct SidebarSpeed;

// Should be a scene, but I was having trouble with EzScene™
fn spawn_sidebar(mut commands: Commands) {
    commands
        .spawn_bundle(NodeBundle {
            style: Style {
                position_type: PositionType::Absolute,
                flex_direction: FlexDirection::RowReverse,
                align_items: AlignItems::FlexEnd,
                size: Size::new(Val::Percent(100.), Val::Percent(100.)),
                ..default()
            },
            color: Color::NONE.into(),
            ..default()
        })
        .with_children(|parent| {
            parent
                .spawn_bundle(NodeBundle {
                    style: Style {
                        flex_direction: FlexDirection::ColumnReverse,
                        align_items: AlignItems::FlexStart,
                        ..default()
                    },
                    color: Color::rgb(0.4, 0., 0.).into(),
                    ..default()
                })
                .with_children(|parent| {
                    parent
                        .spawn_bundle(TextBundle {
                            text: Text::from_section(
                                "",
                                TextStyle {
                                    font_size: 36.,
                                    ..default()
                                },
                            ),
                            ..default()
                        })
                        .insert(SidebarName);

                    let text_style = TextStyle {
                        font_size: 27.,
                        ..default()
                    };

                    parent
                        .spawn_bundle(TextBundle {
                            text: Text::from_section("", text_style.clone()),
                            ..default()
                        })
                        .insert(SidebarHealth);

                    parent
                        .spawn_bundle(TextBundle {
                            text: Text::from_section("", text_style.clone()),
                            ..default()
                        })
                        .insert(SidebarDamage);

                    parent
                        .spawn_bundle(TextBundle {
                            text: Text::from_section("", text_style),
                            ..default()
                        })
                        .insert(SidebarSpeed);
                });
        })
        .insert(Sidebar);
}

fn despawn_combat_ui(
    mut commands: Commands,
    sidebars: Query<Entity, Or<(With<Sidebar>, With<EndTurnWrapper>)>>,
) {
    for sidebar in &sidebars {
        commands.entity(sidebar).despawn_recursive();
    }
}

fn add_sidebar_font(
    mut sidebar_texts: Query<
        &mut Text,
        Or<(
            Added<SidebarName>,
            Added<SidebarHealth>,
            Added<SidebarDamage>,
            Added<SidebarSpeed>,
        )>,
    >,
    assets: Res<UiAssets>,
) {
    for mut text in &mut sidebar_texts {
        text.sections[0].style.font = assets.font_bold.clone();
    }
}

fn update_sidebar(
    mut sidebar_texts: Query<(
        &mut Text,
        AnyOf<(&SidebarName, &SidebarHealth, &SidebarDamage, &SidebarSpeed)>,
    )>,
    hovered_insect_part: Res<HoveredInsectPart>,
    body_parts: Res<BodyParts>,
) {
    for (mut text, sidebar_items) in &mut sidebar_texts {
        text.sections[0].value = if let Some(hovered_insect_part) = **hovered_insect_part {
            let part = &body_parts[hovered_insect_part];
            match sidebar_items {
                (Some(_), _, _, _) => part.pub_name.clone(),
                (_, Some(_), _, _) => format!("Max Health: {}", part.max_health),
                (_, _, Some(_), _) => format!(
                    "Damage: {}",
                    match part.damage {
                        0 => "0".to_string(),
                        damage => format!("{} Adjacent", damage),
                    }
                ),
                (_, _, _, Some(_)) => format!("Speed: {}", part.move_bonus),
                _ => "".to_string(),
            }
        } else {
            "".to_string()
        }
    }
}

#[derive(Component)]
struct EndTurnWrapper;

#[derive(Component)]
struct EndTurnButton;

fn spawn_end_turn_button(mut commands: Commands, assets: Res<UiAssets>) {
    commands
        .spawn_bundle(NodeBundle {
            style: Style {
                position_type: PositionType::Absolute,
                flex_direction: FlexDirection::RowReverse,
                align_items: AlignItems::FlexStart,
                size: Size::new(Val::Percent(100.), Val::Percent(100.)),
                ..default()
            },
            color: Color::NONE.into(),
            ..default()
        })
        .insert(EndTurnWrapper)
        .with_children(|parent| {
            parent
                .spawn_bundle(ButtonBundle {
                    style: Style {
                        flex_direction: FlexDirection::ColumnReverse,
                        align_items: AlignItems::FlexStart,
                        ..default()
                    },
                    color: Color::rgb(0.4, 0., 0.).into(),
                    ..default()
                })
                .with_children(|parent| {
                    parent.spawn_bundle(TextBundle {
                        text: Text::from_section(
                            "END TURN >",
                            TextStyle {
                                font_size: 48.,
                                font: assets.font_bold.clone(),
                                ..default()
                            },
                        ),
                        focus_policy: FocusPolicy::Pass,
                        ..default()
                    });
                })
                .insert(EndTurnButton);
        });
}

fn handle_end_turn_button(
    mut commands: Commands,
    mut squishes: EventWriter<Squish>,
    buttons: Query<&Interaction, (With<EndTurnButton>, Changed<Interaction>)>,
) {
    for button in &buttons {
        if let Interaction::Clicked = button {
            squishes.send(Squish);
            commands.remove_resource::<SelectedUnit>();
            commands.insert_resource(NextState(Turn::animate_goodie()));
        }
    }
}
