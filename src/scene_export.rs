use std::path::{PathBuf, Path};

use iyes_scene_tools::*;

use crate::prelude::*;

/// Send an event of this type to trigger a scene export
pub struct SceneExportEvent {
    pub kind: SceneExportKind,
    pub path: PathBuf,
}

/// What to include in the scene
pub enum SceneExportKind {
    Ui,
    GameLevel,
}

pub struct SceneExportPlugin;

impl Plugin for SceneExportPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<Events<SceneExportEvent>>();
        app.add_system_to_stage(
            CoreStage::Last,
            export_scene_system
                .exclusive_system()
                .at_end()
        );
    }
}

fn export_scene_system(world: &mut World) {
    let mut export_ui = None;
    let mut export_gamelevel = None;

    {
        let mut events = world.resource_mut::<Events<SceneExportEvent>>();
        for ev in events.drain() {
            match ev.kind {
                SceneExportKind::Ui => {
                    export_ui = Some(ev.path);
                }
                SceneExportKind::GameLevel => {
                    export_gamelevel = Some(ev.path);
                }
            }
        }
    }

    if let Some(path) = export_ui {
        export_scene_ui(world, &path);
    }

    if let Some(path) = export_gamelevel {
        export_scene_gamelevel(world, &path);
    }
}

fn export_scene_ui(world: &mut World, path: &Path) {
    let mut builder = SceneBuilder::new(world);

    builder.add_from_query_filter::<
        // (
        //     (Option<&Parent>, Option<&Children>),
        //     (&Style, Option<&UiColor>, Option<&UiImage>, Option<&Text>, Option<&Button>),
        // ),
        With<Node>,
    >();
    builder.ignore_components::<
        (&Transform, &GlobalTransform, &Visibility, &ComputedVisibility, &Node, &CalculatedSize, &Text)
    >();

    if let Err(e) = builder.export_to_file(path) {
        error!("UI Scene export to {:?} failed: {}", path, e);
    }
}

fn export_scene_gamelevel(world: &mut World, path: &Path) {
    let mut builder = SceneBuilder::new(world);


    if let Err(e) = builder.export_to_file(path) {
        error!("Game Level Scene export to {:?} failed: {:#}", path, e);
    }
}
