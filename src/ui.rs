use crate::{
    asset::{UiAssets, UiScenes},
    prelude::*,
};

pub struct UiPlugin;

impl Plugin for UiPlugin {
    fn build(&self, app: &mut App) {
        app.add_enter_system(AppState::MainMenu, setup_main_menu);
        app.add_exit_system(AppState::MainMenu, cleanup_main_menu);
        app.add_system(setup_ui_blueprints.run_not_in_state(AppState::AssetsLoading));
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
    q: Query<(&Interaction, &B), (Changed<Interaction>, With<Button>)>,
) -> Option<B> {
    for (interaction, b) in q.iter() {
        if let Interaction::Clicked = interaction {
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
