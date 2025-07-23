use dotnet_debugger::RustNativeObject;

use geometry::{Rectangle, Vector};
use itertools::Itertools as _;

#[derive(Debug, Clone)]
pub struct TileMap<T> {
    values: Vec<T>,
    height: usize,
    width: usize,
}
impl<T: 'static> RustNativeObject for TileMap<T> {}

impl<T> TileMap<T> {
    #[allow(dead_code)]
    pub fn new(values: Vec<T>, width: usize, height: usize) -> Self {
        Self {
            values,
            height,
            width,
        }
    }

    pub fn height(&self) -> usize {
        self.height
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn full(value: T, width: usize, height: usize) -> Self
    where
        T: Copy,
    {
        Self {
            values: vec![value; width * height],
            height,
            width,
        }
    }

    pub fn empty(width: usize, height: usize) -> Self
    where
        T: Default,
    {
        Self {
            values: (0..height * width).map(|_| Default::default()).collect(),
            height,
            width,
        }
    }

    #[allow(dead_code)]
    pub fn in_bounds(&self, index: impl AsGridPos) -> bool {
        index.get_flat_index(self.width, self.height).is_some()
    }

    pub fn get(&self, index: impl AsGridPos) -> Option<&T> {
        index
            .get_flat_index(self.width, self.height)
            .map(|flat_index| &self.values[flat_index])
    }

    pub fn get_mut(&mut self, index: impl AsGridPos) -> Option<&mut T> {
        index
            .get_flat_index(self.width, self.height)
            .map(|flat_index| &mut self.values[flat_index])
    }

    pub fn iter(&self) -> impl Iterator<Item = (Vector<isize>, &T)> + '_ {
        (0..self.height)
            .cartesian_product(0..self.width)
            .map(|(j, i)| Vector::new(i as isize, j as isize))
            .map(|loc| (loc, &self[loc]))
    }

    pub fn map<Func, U>(&self, func: Func) -> TileMap<U>
    where
        Func: Fn(&T) -> U,
    {
        TileMap {
            values: self.values.iter().map(func).collect(),
            height: self.height,
            width: self.width,
        }
    }

    pub fn imap<Func, U>(&self, func: Func) -> TileMap<U>
    where
        Func: Fn(Vector<isize>, &T) -> U,
    {
        TileMap {
            values: self
                .values
                .iter()
                .enumerate()
                .map(|(i, value)| {
                    let loc = Vector::new(i % self.width, i / self.width)
                        .map(|x| x as isize);
                    func(loc, value)
                })
                .collect(),
            height: self.height,
            width: self.width,
        }
    }

    pub fn shape(&self) -> Vector<isize> {
        Vector::new(self.width as isize, self.height as isize)
    }

    pub fn bounds(&self) -> Rectangle<isize> {
        Rectangle {
            top_left: Vector::zero(),
            shape: self.shape(),
        }
    }
}

impl TileMap<bool> {
    pub fn collect_true<Iter, Index>(
        shape: Vector<isize>,
        iter_tiles: Iter,
    ) -> Self
    where
        Iter: IntoIterator<Item = Index>,
        Index: AsGridPos,
    {
        let mut map = Self::empty(shape.right as usize, shape.down as usize);
        for tile in iter_tiles {
            if let Some(value) = map.get_mut(tile) {
                *value = true;
            }
        }

        map
    }

    pub fn collect_false<Iter, Index>(
        shape: Vector<isize>,
        iter_tiles: Iter,
    ) -> Self
    where
        Iter: IntoIterator<Item = Index>,
        Index: AsGridPos,
    {
        let mut map =
            Self::full(true, shape.right as usize, shape.down as usize);
        for tile in iter_tiles {
            if let Some(value) = map.get_mut(tile) {
                *value = false;
            }
        }

        map
    }

    pub fn iter_true(&self) -> impl Iterator<Item = Vector<isize>> + '_ {
        self.iter().filter(|(_, b)| **b).map(|(tile, _)| tile)
    }

    pub fn iter_false(&self) -> impl Iterator<Item = Vector<isize>> + '_ {
        self.iter().filter(|(_, b)| !**b).map(|(tile, _)| tile)
    }

    pub fn is_set(&self, index: impl AsGridPos) -> bool {
        self.get(index).cloned().unwrap_or(false)
    }

    pub fn is_unset(&self, index: impl AsGridPos) -> bool {
        self.get(index).map(|value| !*value).unwrap_or(false)
    }

    pub fn dilate(&self, structure: &TileMap<bool>) -> Self {
        let new_shape = Vector::new(
            self.width() + structure.width() - 1,
            self.height() + structure.height() - 1,
        )
        .map(|d| d as isize);

        Self::collect_true(
            new_shape,
            self.iter_true().flat_map(|tile| {
                structure.iter_true().map(move |offset| tile + offset)
            }),
        )
    }

    pub fn erode(&self, structure: &TileMap<bool>) -> Self {
        let new_shape = Vector::new(
            self.width() - (structure.width() - 1),
            self.height() - (structure.height() - 1),
        )
        .map(|d| d as isize);

        Self::collect_true(
            new_shape,
            (0..new_shape.right)
                .flat_map(|i| {
                    (0..new_shape.down).map(move |j| Vector::new(i, j))
                })
                .filter(|&tile| {
                    structure
                        .iter_true()
                        .all(|offset| self.is_set(tile + offset))
                }),
        )
    }

    pub fn close_over(&self, structure: &TileMap<bool>) -> Self {
        self.dilate(structure).erode(structure)
    }

    fn normalize_grid_pos(
        &self,
        point: impl AsGridPos,
    ) -> Option<Vector<isize>> {
        let flat = point.get_flat_index(self.width, self.height)?;
        Some(Vector::new(
            (flat % self.width) as isize,
            (flat / self.width) as isize,
        ))
    }

    pub fn floodfill(&self, initial: impl AsGridPos) -> Self {
        let mut output = Self::empty(self.width, self.height);
        let mut to_visit = Vec::new();

        if let Some(initial) = self
            .normalize_grid_pos(initial)
            .filter(|tile| !self.is_set(*tile))
        {
            output[initial] = true;
            to_visit.push(initial);
        }

        while let Some(visiting) = to_visit.pop() {
            for adj in visiting.iter_cardinal() {
                if output.in_bounds(adj)
                    && !self.is_set(adj)
                    && !output.is_set(adj)
                {
                    output[adj] = true;
                    to_visit.push(adj);
                }
            }
        }

        output
    }
}

impl<T> TileMap<Option<T>> {
    pub fn is_some(&self, index: impl AsGridPos) -> bool {
        self.get(index).map(|opt| opt.is_some()).unwrap_or(false)
    }

    pub fn is_none(&self, index: impl AsGridPos) -> bool {
        self.get(index).map(|opt| opt.is_none()).unwrap_or(false)
    }

    pub fn get_opt(&self, index: impl AsGridPos) -> Option<&T> {
        self.get(index)?.as_ref()
    }
}

pub trait AsGridPos {
    fn get_flat_index(&self, width: usize, height: usize) -> Option<usize>;

    fn display(&self) -> impl std::fmt::Debug;
}
impl AsGridPos for (isize, isize) {
    fn get_flat_index(&self, width: usize, height: usize) -> Option<usize> {
        let (i, j) = *self;
        if i < 0 || j < 0 {
            return None;
        }

        let i = i as usize;
        let j = j as usize;
        (i, j).get_flat_index(width, height)
    }

    fn display(&self) -> impl std::fmt::Debug {
        self
    }
}
impl AsGridPos for (i32, i32) {
    fn get_flat_index(&self, width: usize, height: usize) -> Option<usize> {
        (self.0 as isize, self.1 as isize).get_flat_index(width, height)
    }

    fn display(&self) -> impl std::fmt::Debug {
        self
    }
}
impl AsGridPos for (usize, usize) {
    fn get_flat_index(&self, width: usize, height: usize) -> Option<usize> {
        let (i, j) = *self;
        (i < width && j < height).then(|| j * width + i)
    }

    fn display(&self) -> impl std::fmt::Debug {
        self
    }
}
impl AsGridPos for Vector<usize> {
    fn get_flat_index(&self, width: usize, height: usize) -> Option<usize> {
        (self.right, self.down).get_flat_index(width, height)
    }

    fn display(&self) -> impl std::fmt::Debug {
        (self.right, self.down)
    }
}
impl AsGridPos for Vector<isize> {
    fn get_flat_index(&self, width: usize, height: usize) -> Option<usize> {
        (self.right, self.down).get_flat_index(width, height)
    }

    fn display(&self) -> impl std::fmt::Debug {
        (self.right, self.down)
    }
}

impl<I, T> std::ops::Index<I> for TileMap<T>
where
    I: AsGridPos,
{
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        let index = index
            .get_flat_index(self.width, self.height)
            .unwrap_or_else(|| {
                panic!(
                    "Index {:?} is out-of-bounds \
                     for a TileMap of shape ({}, {})",
                    index.display(),
                    self.width,
                    self.height,
                )
            });

        &self.values[index]
    }
}

impl<I, T> std::ops::IndexMut<I> for TileMap<T>
where
    I: AsGridPos,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        let index = index
            .get_flat_index(self.width, self.height)
            .unwrap_or_else(|| {
                panic!(
                    "Index {:?} is out-of-bounds \
                     for a TileMap of shape ({}, {})",
                    index.display(),
                    self.width,
                    self.height,
                )
            });

        &mut self.values[index]
    }
}

impl std::fmt::Display for TileMap<char> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for j in 0..self.height {
            for i in 0..self.width {
                write!(fmt, "{}", self[(i, j)])?;
            }
            if j + 1 < self.height {
                write!(fmt, "\n")?;
            }
        }
        Ok(())
    }
}
