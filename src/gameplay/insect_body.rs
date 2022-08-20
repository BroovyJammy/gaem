use bevy::prelude::*;
use std::collections::HashSet;

pub enum Rotation {
    Up,
    Down,
    Left,
    Right,
}
pub enum InsectPartKind {
    Flesh,
    Head,
    Legs,
}
pub struct InsectPart {
    pub position: (u32, u32),
    pub kind: InsectPartKind,
    pub rotation: Rotation,
}

pub struct InsectBody {
    pub parts: Box<[InsectPart]>,
    pub used_tiles: HashSet<(u32, u32)>,
}
