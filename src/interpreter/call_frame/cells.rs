use std::{
    fmt,
    ops::{Deref, DerefMut},
};

use thin_vec::ThinVec;

use crate::interpreter::stm::VarId;

/// Captured cell identities, with a single cell stored inside the frame.
#[derive(Clone, Default)]
pub enum FrameCells {
    #[default]
    Empty,
    One(VarId),
    Many(ThinVec<VarId>),
}

impl FrameCells {
    pub(super) fn with_new(self, count: usize) -> Self {
        match (self, count) {
            (cells, 0) => cells,
            (Self::Empty, 1) => Self::One(VarId::new()),
            (cells, count) => {
                let mut cells = match cells {
                    Self::Empty => ThinVec::with_capacity(count),
                    Self::One(cell) => {
                        let mut cells = ThinVec::with_capacity(count + 1);
                        cells.push(cell);
                        cells
                    }
                    Self::Many(mut cells) => {
                        cells.reserve_exact(count);
                        cells
                    }
                };
                cells.extend((0..count).map(|_| VarId::new()));
                Self::Many(cells)
            }
        }
    }
}

impl From<&[VarId]> for FrameCells {
    fn from(cells: &[VarId]) -> Self {
        match cells {
            [] => Self::Empty,
            [cell] => Self::One(*cell),
            cells => Self::Many(cells.into()),
        }
    }
}

impl From<ThinVec<VarId>> for FrameCells {
    fn from(cells: ThinVec<VarId>) -> Self {
        match cells.as_slice() {
            [] => Self::Empty,
            [cell] => Self::One(*cell),
            _ => Self::Many(cells),
        }
    }
}

impl Deref for FrameCells {
    type Target = [VarId];

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Empty => &[],
            Self::One(cell) => std::slice::from_ref(cell),
            Self::Many(cells) => cells,
        }
    }
}

impl DerefMut for FrameCells {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Self::Empty => &mut [],
            Self::One(cell) => std::slice::from_mut(cell),
            Self::Many(cells) => cells,
        }
    }
}

impl fmt::Debug for FrameCells {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.deref().fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nested_calls_keep_inherited_cells_before_fresh_cells() {
        let parent = FrameCells::default().with_new(1);
        let child = FrameCells::from(&*parent).with_new(2);
        assert_eq!(child[0], parent[0]);
        assert_ne!(child[1], child[0]);
        assert_ne!(child[2], child[0]);
        assert_ne!(child[1], child[2]);
        let nested = child.clone().with_new(1);
        assert_eq!(&nested[..3], &*child);
        assert!(!child.contains(&nested[3]));
    }

    #[test]
    fn rebinding_does_not_change_captured_identities() {
        for count in [1, 3] {
            let mut cells = FrameCells::default().with_new(count);
            let captured = cells.clone();
            cells[0] = VarId::new();
            assert_ne!(cells[0], captured[0]);
            assert_eq!(&cells[1..], &captured[1..]);
        }
    }
}
